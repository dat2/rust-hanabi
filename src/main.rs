#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate ws;
extern crate serde;
extern crate serde_json;

use std::rc::Rc;
use ws::{listen,Message,Sender};
use serde_json::{Value, Map};

#[derive(Debug, PartialEq)]
enum Event
{
  Init
}

impl serde::Serialize for Event
{
  fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
    where S: serde::Serializer
  {
    let human_message = match self {
      &Event::Init => "Init"
    };

    serializer.serialize_str(human_message)
  }
}

impl serde::Deserialize for Event
{
  fn deserialize<D>(deserializer: &mut D) -> Result<Event, D::Error>
    where D: serde::de::Deserializer
  {

    struct EventVisitor;
    impl serde::de::Visitor for EventVisitor
    {
      type Value = Event;

      fn visit_str<E>(&mut self, value: &str) -> Result<Event, E>
        where E: serde::de::Error,
      {
        match value {
          "Init" => Ok(Event::Init),
          _ => Err(serde::de::Error::custom("Wrong event type"))
        }
      }
    }

    deserializer.deserialize(EventVisitor)
  }
}


#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct JsonMessage
{
  event: Event,
  data: Value
}

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
  // the listener address
  let bind = "127.0.0.1:3012";

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
