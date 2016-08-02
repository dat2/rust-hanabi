#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]
#![plugin(dotenv_macros)]

// import external
extern crate serde;
extern crate serde_json;
extern crate ws;
extern crate dotenv;

// import local
mod messages;

use dotenv::dotenv;
use std::rc::Rc;
use ws::{listen,Message,Sender};
use serde_json::{Value, Map};
use messages::{JsonMessage,Event};

// cases we need to handle
// intermittent client connections

// return a string to send back to the client
fn handle_init(v: &Value, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let mut map = Map::new();
  map.insert("test".to_string(), Value::I64(123));
  let response = JsonMessage { event: Event::Init, data: Value::Object(map) };

  // return the string to send
  let response = serde_json::to_string(&response).unwrap();
  out.send(response)
}

fn main()
{
  dotenv().ok();

  // the listener address
  let bind = dotenv!("BIND");
  println!("{}", dotenv!("BIND"));

  // the main server handler
  listen(bind, |out|
  {
    let out = Rc::new(out);

    move |msg: Message|
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

      match message.event {
        Event::Init => handle_init(&message.data, out.clone())
      }
    }
  }).unwrap()
}
