import React from 'react';
import { Provider } from 'react-redux';
import { Router } from 'react-router';
import DevTools from './DevTools';
import makeRoutes from '../routes';

const Root = ({ store, history }) => (
  <Provider store={store}>
    <div className='root'>
      <Router history={history} routes={makeRoutes(store)}/>
      <DevTools />
    </div>
  </Provider>
);

export default Root;
