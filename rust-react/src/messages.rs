use game::{GameState};
use serde;
use serde_json;
use std::rc::Rc;
use ws;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Event
{
  SetName(String),

  GetChannels,
  CreateChannel(String),
  JoinChannel(String),
  LeaveChannel,

  StartGame,
  SendMessage(String),

  // The rest here is for us sending to the client, and we will never receive it
  SendChannels(Vec<String>),
  SendState(GameState),
  Error(String)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct JsonMessage
{
  pub payload: Event
}

pub fn serialize_response<T> (response: T, out: Rc<ws::Sender>) -> Result<(), ws::Error>
  where T: serde::Serialize
{
  match serde_json::to_string(&response)
  {
    Ok(response) =>
    {
      println!("Sending {:?}", response);
      out.send(response)
    },
    Err(e) =>
    {
      println!("Error while serializing response, {:?}", e);
      Ok(())
    }
  }
}
