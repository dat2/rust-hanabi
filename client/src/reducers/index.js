import { routerReducer as routing } from 'react-router-redux';
import { combineReducers } from 'redux';

import user from '../modules/user';
import channels from '../modules/channels';

const rootReducer = combineReducers({
  routing,
  user,
  channels
});

export default rootReducer;
