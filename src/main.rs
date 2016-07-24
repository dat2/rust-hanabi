#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate ws;
extern crate serde;
extern crate serde_json;

use ws::{listen,Message,Sender};
use serde_json::{Value, Map};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct JsonMessage
{
  event: String,
  data: Value
}

// cases we need to handle
// intermittent client connections

fn handleHey<'a>(v: &Value, out: Rc<Sender>) -> Result<(), ws::Error>
{
  let mut map = Map::new();
  map.insert("test".to_string(), Value::I64(123));
  let response = JsonMessage { event: "hey".to_string(), data: Value::Object(map) };
  let response_string = serde_json::to_string(&response).unwrap();

  out.send(response_string)
}

fn noHandlersFound() -> Result<(), ws::Error> {
  println!("No handler registered for this event!");
  Ok(())
}

fn main()
{
  // the listener address
  let bind = "127.0.0.1:3012";

  // the main game state
  // let mut game = Game { players: vec![] };

  let mut handlers = HashMap::new();
  handlers.insert("hey".to_string(), handleHey);
  let handlers = handlers;

  // the main server handler
  listen(bind, |out|
  {
    let out = Rc::new(out);
    move |msg: Message|
    {

      // parse the received message
      let json_string = msg.as_text().unwrap();
      let message: JsonMessage = serde_json::from_str(&json_string).unwrap();
      println!("Message received {:?}", message);

      match message.event.as_ref() {
        "hey" => {
          handleHey(&message.data, out.clone())
        },
        _ => {
          noHandlersFound()
        }
      }

    }
  }).unwrap()
}
