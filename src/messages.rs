use game::{GameState};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Event
{
  SetName(String),
  GetChannels,
  CreateChannel(String),
  JoinChannel(String),
  LeaveChannel,
  StartGame,

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
