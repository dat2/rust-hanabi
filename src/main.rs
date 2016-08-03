#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]
#![plugin(dotenv_macros)]

// import external
extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate ws;
extern crate dotenv;

// import local
mod messages;
mod game;

use game::{GameState};
use dotenv::dotenv;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap,BTreeMap};
use ws::{listen,Message,Sender};
use serde_json::{Value, to_value};
use messages::{JsonMessage,Event};

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
    Ok(response) => out.send(response),
    Err(e) => {
      println!("Error while serializing response, {:?}", e);
      Ok(())
    }
  }
}

// return a string to send back to the client
fn handle_init(_: &Value, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let game = GameState::new(5, 3);
  let response = JsonMessage { event: Event::Init, data: to_value(&game) };

  // return the string to send
  serialize_response(response, out)
}

type ChannelName = String;
type Registry = Rc<RefCell<HashMap<u64, Rc<Sender>>>>;

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
    match serde_json::from_str(&json_string) {
      Ok(m) => {
        println!("Message received {:?}", m);
        // match m.event {
        //   Event::Init => handle_init(&m.data, self.out.clone())
        // }
        return Ok(());
      },

      // print an error
      Err(e) => {
        println!("The message received is not valid {:?}", e);
        return Ok(());
      }
    };
  }

  fn on_close(&mut self, _: ws::CloseCode, _: &str)
  {
    self.registry.borrow_mut().remove(&self.id);
  }
}
