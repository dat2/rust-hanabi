import React from 'react';
import { Router, Route, hashHistory } from 'react-router';

import App from './App';
import Lobby from './Lobby';
import NameInput from './NameInput';

export default () => (
  <Router history={hashHistory}>
    <Route path='/' component={App}>
      <Route path='lobby' component={Lobby} />
      <Route path='name' component={NameInput} />
    </Route>
  </Router>
);
