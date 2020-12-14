import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import "regenerator-runtime/runtime.js";
// The imported methods will use the File System
// Access API or a fallback implementation.
import {
  fileOpen,
  directoryOpen,
  fileSave,
} from 'browser-nativefs';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.addSimpleChar.subscribe(function () {
  getSimpleChar();
});


function getSimpleChar() {
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
      var simpleChars = {};
      files.forEach(function (file) {
        var reader = new FileReader();
        reader.addEventListener('load', function (event) {
          var char = file.name.slice(0, -(".svg".length));
          simpleChars[char] = event.target.result;
          if (Object.keys(simpleChars).length === files.length) {
            console.log(simpleChars);
            app.ports.getSimpleChar.send(simpleChars);
          }
        });
        reader.readAsText(file);
      });
    });
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
