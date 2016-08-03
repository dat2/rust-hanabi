use serde;
use serde_json::{Value};

// event string names
const INIT: &'static str = "Init";

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
      &Event::Init => INIT
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
          INIT => Ok(Event::Init),
          _ => Err(serde::de::Error::custom("Wrong event type"))
        }
      }
    }

    deserializer.deserialize(EventVisitor)
  }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct JsonMessage<T: serde::Serialize>
{
  pub event: Event,
  pub data: T
}
