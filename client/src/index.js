import 'bulma';
import './base.scss';

import React from 'react';
import ReactDOM from 'react-dom';
import Root from './containers/Root';

import configureStore from './store/configureStore';
const store = configureStore();

import { browserHistory } from 'react-router';
import { syncHistoryWithStore } from 'react-router-redux';
const history = syncHistoryWithStore(browserHistory, store);

ReactDOM.render(
  <Root store={store} history={history} />,
  document.getElementById('root')
);
