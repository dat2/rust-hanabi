#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]
#![plugin(dotenv_macros)]
#![plugin(phf_macros)]

// import external
extern crate serde;
extern crate serde_json;
extern crate ws;
extern crate dotenv;
extern crate rand;
extern crate chrono;
#[macro_use]
extern crate phf;

use dotenv::dotenv;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use ws::{listen,Message,Sender};

// import local
mod messages;
mod game;
use game::{GameState, Channel};
use messages::{JsonMessage,Event};

type Registry = Rc<RefCell<HashMap<u64, Rc<Sender>>>>;

// https://github.com/housleyjk/ws-rs/issues/56#issuecomment-231497839
fn main()
{
  dotenv().ok();

  let registry: Registry = Rc::new(RefCell::new(HashMap::new()));
  let mut id_counter = 0;

  // the main server handler
  listen(dotenv!("BIND"), |out|
  {
    id_counter = id_counter + 1;

    Handler
    {
      id: id_counter,
      out: Rc::new(out),
      registry: registry.clone(),
    }
  }).unwrap()
}

// cases we need to handle
// intermittent client connections

fn serialize_response<T> (response: T, out: Rc<Sender>) -> Result<(), ws::Error>
  where T: serde::Serialize
{
  match serde_json::to_string(&response) {
    Ok(response) => {
      println!("Sending {:?}", response);
      out.send(response)
    },
    Err(e) => {
      println!("Error while serializing response, {:?}", e);
      Ok(())
    }
  }
}

// return a string to send back to the client
fn handle_init(out: Rc<Sender>) -> Result<(), ws::Error>
{
  let response = JsonMessage { data: Event::GetState(GameState::new()) };
  serialize_response(response, out)
}

struct Handler
{
  id: u64,
  out: Rc<Sender>,
  registry: Registry
}

impl ws::Handler for Handler {
  fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()>
  {
    self.registry.borrow_mut().insert(self.id, self.out.clone());
    Ok(())
  }

  fn on_message(&mut self, msg: Message) -> ws::Result<()>
  {
    // parse the received message
    let json_string = msg.as_text().unwrap();

    // try unwrapping if possible, else print an error
    let message: JsonMessage = match serde_json::from_str(&json_string) {
      Ok(m) => m,

      // print an error
      Err(e) => {
        println!("The message received is not valid {:?}", e);
        return Ok(());
      }
    };

    println!("Message received {:?}", message);

    match message.data {
      Event::Init => handle_init(self.out.clone()),
      _ => Ok(())
    }
  }

  fn on_close(&mut self, _: ws::CloseCode, _: &str)
  {
    self.registry.borrow_mut().remove(&self.id);
  }
}
