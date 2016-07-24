extern crate serde;
extern crate serde_json;

use serde_json::{Value};

#[derive(Debug, PartialEq)]
pub enum Event
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
pub struct JsonMessage
{
  pub event: Event,
  pub data: Value
}
