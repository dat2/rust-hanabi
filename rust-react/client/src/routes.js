import React from 'react';
import { IndexRoute, Route } from 'react-router';

import App from './components/App';
import Lobby from './components/Lobby';
import WelcomePage from './components/WelcomePage';
import Game from './components/Game';

// if the user has not set their name on the welcome page, redirect them there
const requireName = store => (nextState, replace) => {
  const { user } = store.getState();
  if(!user.name) {
    replace('/');
  }
};

export default (store) => (
  <Route path='/' component={App}>
    <IndexRoute component={WelcomePage} />
    <Route path='lobby' component={Lobby} onEnter={requireName(store)} />
    <Route path='game/:name' component={Game} onEnter={requireName(store)} />
  </Route>
);
