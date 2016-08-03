use game::{GameState};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Event
{
  Init,
  // The rest here is for us sending to the client, and we will never receive it
  GetState(GameState)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct JsonMessage
{
  pub data: Event
}
