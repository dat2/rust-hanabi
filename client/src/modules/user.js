import { createAction, handleActions } from 'redux-actions';

// action types
const SET_NAME = 'SET_NAME';
const JOIN_CHANNEL = 'JOIN_CHANNEL';

// actions
export const setName = createAction(SET_NAME);
export const joinChannel = createAction(JOIN_CHANNEL);

// reducer
export const initialState = {
  name: '',
  channel: ''
};
const userReducer = handleActions({
  [SET_NAME]: (state, action) => ({
    ...state,
    name: action.payload
  }),
}, initialState);

export default userReducer;
