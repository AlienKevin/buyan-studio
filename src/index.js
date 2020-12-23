import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import localforage from 'localforage';

var storageKey = 'buyan-studio-model';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.saveModelPort.subscribe(function (model) {
  console.log("Saving model: ", model);
  localforage.setItem(storageKey, model, function (error) {
    if (error !== null) {
      console.error("error saving model: ", error);
    } else {
      console.log("Successfully saved model!");
    }
  });
});

localforage.getItem(storageKey, function(error, savedModelJson) {
  if (error !== null) {
    console.error("Error getting saved model: ", error);
  }
  console.log("Getting saved model: ", savedModelJson);
  app.ports.getModelPort.send(savedModelJson);
});

window.addEventListener("beforeunload", function() {
  app.ports.pageUnloadingPort.send(null);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
