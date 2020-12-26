import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import localforage from 'localforage';
import "regenerator-runtime/runtime.js";
// The imported methods will use the File System
// Access API or a fallback implementation.
import { fileOpen } from 'browser-nativefs';

var baseStorageKey = 'buyan-studio-';
var modelStorageKey = baseStorageKey + 'model';
var simpleCharSvgsStorageKey = baseStorageKey + 'simpleCharSvgs'

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.addSimpleCharsPort.subscribe(function () {
  var options = {
    // List of allowed MIME types, defaults to `*/*`.
    mimeTypes: ['image/svg+xml'],
    // List of allowed file extensions (with leading '.'), defaults to `''`.
    extensions: ['.svg'],
    // Set to `true` for allowing multiple files, defaults to `false`.
    multiple: true,
    // Textual description for file dialog , defaults to `''`.
    description: 'Simple Character SVGs',
  };
  fileOpen(options)
    .then(function (files) {
      console.log(files);
      localforage.getItem(simpleCharSvgsStorageKey, function (error, simpleCharSvgs) {
        if (error !== null) {
          console.error("Error getting saved simpleCharSvgs: ", error);
        }
        if (simpleCharSvgs === null) {
          simpleCharSvgs = {};
        }
        var numberOfNewCharsAdded = 0;
        Array.from(files).forEach(function (file) {
          var reader = new FileReader();
          reader.addEventListener('load', function (event) {
            var char = file.name.slice(0, -(".svg".length));
            simpleCharSvgs[char] = event.target.result;
            numberOfNewCharsAdded += 1;
            if (numberOfNewCharsAdded === files.length) {
              app.ports.getSimpleCharsPort.send(simpleCharSvgs);
              localforage.setItem(simpleCharSvgsStorageKey, simpleCharSvgs, function (error) {
                if (error !== null) {
                  console.error("Error saving simpleCharSvgs: ", error);
                }
              });
            }
          });
          reader.readAsText(file);
        });
      });
    });
});

app.ports.deleteSimpleCharPort.subscribe(function(char) {
  localforage.getItem(simpleCharSvgsStorageKey, function (error, simpleCharSvgs) {
    if (error !== null) {
      console.error("Error getting saved simpleCharSvgs: ", error);
    }
    delete simpleCharSvgs[char];
    localforage.setItem(simpleCharSvgsStorageKey, simpleCharSvgs, function (error) {
      if (error !== null) {
        console.error("Error saving simpleCharSvgs: ", error);
      }
    });
  });
});

app.ports.saveModelPort.subscribe(function (model) {
  console.log("Saving model: ", model);
  localforage.setItem(modelStorageKey, model, function (error) {
    if (error !== null) {
      console.error("error saving model: ", error);
    } else {
      console.log("Successfully saved model!");
    }
  });
});

localforage.getItem(modelStorageKey, function (error, savedModelJson) {
  if (error !== null) {
    console.error("Error getting saved model: ", error);
  }
  console.log("Getting saved model: ", savedModelJson);
  app.ports.getModelPort.send(savedModelJson);
});

localforage.getItem(simpleCharSvgsStorageKey, function (error, savedSimpleCharSvgs) {
  if (error !== null) {
    console.error("Error getting saved simpleCharSvgs: ", error);
  }
  console.log("Getting saved simpleCharSvgs: ", savedSimpleCharSvgs);
  app.ports.getSimpleCharsPort.send(savedSimpleCharSvgs);
});

window.addEventListener("beforeunload", function () {
  app.ports.pageUnloadingPort.send(null);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
