#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate ws;
extern crate serde;
extern crate serde_json;

use ws::{listen,Message};
use serde_json::{Value, Map};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct JsonMessage
{
  event: String,
  data: Value
}

// cases we need to handle
// intermittent client connections

// return a string to send back to the client
fn handleHey(v: &Value) -> String
{
  let mut map = Map::new();
  map.insert("test".to_string(), Value::I64(123));
  let response = JsonMessage { event: "hey".to_string(), data: Value::Object(map) };

  // return the string to send
  serde_json::to_string(&response).unwrap()
}

fn noHandlersFound() -> String {
  println!("No handler registered for this event!");
  String::new()
}

fn main()
{
  // the listener address
  let bind = "127.0.0.1:3012";

  // the main game state
  // let mut game = Game { players: vec![] };

  let mut handlers = HashMap::new();
  handlers.insert("hey".to_string(), handleHey);

  // the main server handler
  listen(bind, |out|
  {
    move |msg: Message|
    {
      // parse the received message
      let json_string = msg.as_text().unwrap();
      let message: JsonMessage = serde_json::from_str(&json_string).unwrap();
      println!("Message received {:?}", message);

      let response_string = match handlers.get(&message.event) {
        Some(func) => func(&message.data),
        None => noHandlersFound()
      };

      // TODO check if response_string is empty
      out.send(Message::Text(response_string))
    }
  }).unwrap()
}
