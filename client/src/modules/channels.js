import { createAction, handleActions } from 'redux-actions';

// action types
const FETCH_CHANNELS_REQUESTED = 'FETCH_CHANNELS_REQUESTED';
const FETCH_CHANNELS = 'FETCH_CHANNELS';

// actions
export const createFetchChannelsRequestedAction = createAction(FETCH_CHANNELS_REQUESTED);
export const createFetchChannelsAction = createAction(FETCH_CHANNELS);

// reducer
export const initialState = {
  loading: false,
  error: null,
  channels: []
};
const channelReducer = handleActions({
  [FETCH_CHANNELS]: (state, action) => ({
    loading: false,
    error: action.error ? action.payload : null,
    channels: action.error ? state.channels : action.payload
  }),

  [FETCH_CHANNELS_REQUESTED]: (state, action) => ({
    channels: [...state.channels],
    error: null,
    loading: true
  })
}, initialState);

export default channelReducer;
