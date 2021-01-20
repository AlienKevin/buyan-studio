import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import localforage from 'localforage';
import "regenerator-runtime/runtime.js";
// The imported methods will use the File System
// Access API or a fallback implementation.
import { fileOpen, fileSave } from 'browser-nativefs';

var baseStorageKey = 'buyan-studio-';
var modelStorageKey = baseStorageKey + 'model';
var simpleCharSvgsStorageKey = baseStorageKey + 'simpleCharSvgs'
var backupFileHandleStorageKey = baseStorageKey + 'backupFileHandle';
var backupFileHandle;

var browserLanguage = "LanguageEn";
var tag = navigator.language;
if (tag === "zh-CN") {
  browserLanguage = "LanguageZhHans"
} else if (tag.startsWith("zh")) {
  browserLanguage = "LanguageZhHant"
}


localforage.getItem(modelStorageKey, function (error, savedModelJson) {
  if (error !== null) {
    console.error("Error getting saved model: ", error);
  }

  var preferredLanguage = savedModelJson === null ? browserLanguage : savedModelJson.language;
  // console.log("Getting saved model: ", savedModelJson);
  fetch(`translations/${preferredLanguage}.json`)
    .then(function (response) {
      return response.json();
    })
    .catch(function () {
      console.log("Error getting translations");
    })
    .then(function (translations) {
      var app = Elm.Main.init({
        node: document.getElementById('root'),
        flags: {
          language: preferredLanguage,
          translations: translations,
          windowWidth: window.innerWidth,
          windowHeight: window.innerHeight,
        },
      });

      if (savedModelJson !== null) {
        app.ports.getModelPort.send(savedModelJson);
      }

      localforage.getItem(backupFileHandleStorageKey, async function (error, handle) {
        if (error !== null) {
          console.error("Error getting backupFileHandle: ", error);
          return;
        }
        if (handle === null) {
          return;
        }
        backupFileHandle = handle;
        app.ports.succeededInBackupPort.send(null);
      });

      async function verifyPermission(fileHandle) {
        const options = {
          mode: 'readwrite'
        };
        // Check if permission was already granted. If so, return true.
        if ((await fileHandle.queryPermission(options)) === 'granted') {
          return true;
        }
        // Request permission. If the user grants permission, return true.
        if ((await fileHandle.requestPermission(options)) === 'granted') {
          return true;
        }
        // The user didn't grant permission, so return false.
        return false;
      }

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
            // console.log(files);
            localforage.getItem(simpleCharSvgsStorageKey, function (error, simpleCharSvgs) {
              if (error !== null) {
                console.error("Error getting saved simpleCharSvgs: ", error);
              }
              if (simpleCharSvgs === null) {
                simpleCharSvgs = {};
              }
              var newSimpleCharSvgs = {};
              var numberOfNewCharsAdded = 0;
              Array.from(files).forEach(function (file) {
                var reader = new FileReader();
                reader.addEventListener('load', function (event) {
                  var char = file.name.slice(0, -(".svg".length));
                  newSimpleCharSvgs[char] = event.target.result;
                  numberOfNewCharsAdded += 1;
                  if (numberOfNewCharsAdded === files.length) {
                    app.ports.gotNewSimpleCharsPort.send(newSimpleCharSvgs);
                    localforage.setItem(simpleCharSvgsStorageKey
                      , Object.assign(simpleCharSvgs, newSimpleCharSvgs), function (error) {
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

      app.ports.uploadSimpleCharPort.subscribe(function (char) {
        var options = {
          // List of allowed MIME types, defaults to `*/*`.
          mimeTypes: ['image/svg+xml'],
          // List of allowed file extensions (with leading '.'), defaults to `''`.
          extensions: ['.svg'],
          // Set to `true` for allowing multiple files, defaults to `false`.
          multiple: false,
          // Textual description for file dialog , defaults to `''`.
          description: 'SVG for ' + char,
        };
        fileOpen(options)
          .then(function (file) {
            // console.log(files);
            localforage.getItem(simpleCharSvgsStorageKey, function (error, simpleCharSvgs) {
              if (error !== null) {
                console.error("Error getting saved simpleCharSvgs: ", error);
              }
              if (simpleCharSvgs === null) {
                simpleCharSvgs = {};
              }
              var reader = new FileReader();
              reader.addEventListener('load', function (event) {
                var svg = event.target.result;
                simpleCharSvgs[char] = svg;
                app.ports.loadedSimpleCharPort.send(svg);
                localforage.setItem(simpleCharSvgsStorageKey
                  , simpleCharSvgs, function (error) {
                    if (error !== null) {
                      console.error("Error saving simpleCharSvgs: ", error);
                    }
                  });
              });
              reader.readAsText(file);
            });
          });
      });

      app.ports.deleteSimpleCharPort.subscribe(function (char) {
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

      app.ports.clearSimpleCharsPort.subscribe(function () {
        localforage.getItem(simpleCharSvgsStorageKey, function (error, simpleCharSvgs) {
          if (error !== null) {
            console.error("Error getting saved simpleCharSvgs: ", error);
          }
          localforage.setItem(simpleCharSvgsStorageKey, {}, function (error) {
            if (error !== null) {
              console.error("Error saving simpleCharSvgs: ", error);
            }
          });
        });
      });

      app.ports.downloadCharPort.subscribe(function (char) {
        var svgData = document.getElementById("char-" + char).outerHTML;
        //add name spaces.
        if (!svgData.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)) {
          svgData = svgData.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
        }
        if (!svgData.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)) {
          svgData = svgData.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
        }
        var svgBlob = new Blob([svgData], { type: "image/svg+xml;charset=utf-8" });
        var svgUrl = URL.createObjectURL(svgBlob);
        var downloadLink = document.createElement("a");
        downloadLink.href = svgUrl;
        downloadLink.download = char + ".svg";
        document.body.appendChild(downloadLink);
        downloadLink.click();
        document.body.removeChild(downloadLink);
      });

      app.ports.saveModelPort.subscribe(function (model) {
        // console.log("Saving model: ", model);
        localforage.setItem(modelStorageKey, model, function (error) {
          if (error !== null) {
            console.error("error saving model: ", error);
          } else {
            // console.log("Successfully saved model!");
          }
        });
      });

      app.ports.backupAsLocalFilePort.subscribe(backupAsLocalFile, function () { });

      function backupAsLocalFile(model, callback) {
        localforage.getItem(simpleCharSvgsStorageKey, async function (error, simpleCharSvgs) {
          if (error !== null) {
            console.error("Error getting saved simpleCharSvgs: ", error);
            return;
          }
          if (simpleCharSvgs === null) {
            return;
          }
          console.log("Backing up as local file...");
          var json =
          {
            model: model
            , simpleCharSvgs: simpleCharSvgs
          };
          var jsonString = JSON.stringify(json, null, 2);
          var blob = new Blob(
            [jsonString]
            , { type: 'application/json' }
          );
          if (!(await verifyPermission(backupFileHandle))) {
            return;
          }
          fileSave(blob, undefined, backupFileHandle)
            .then(callback);
        });
      }

      app.ports.updateBackupLocationPort.subscribe(function (model) {
        getBackupFileHandle(
          FilePickerType.Save,
          function() {
            backupAsLocalFile(model, function () {
              app.ports.succeededInBackupPort.send(null);
            });
          }
        )
      });

      var FilePickerType =
        {
          "Open": "Open",
          "Save": "Save",
        };

      function getBackupFileHandle(filePickerType, callback) {
        var options = {
          types: [
            {
              description: "Buyan Studio Backup File",
              accept: {
                "application/json": [".json"],
              },
            },
          ],
        };
        (filePickerType === "Open"
        ? window.showOpenFilePicker
        : window.showSaveFilePicker
        )(options)
          .then(function(handle) {
            backupFileHandle = (filePickerType === "Open"
            ? handle[0]
            : handle
            );
            localforage.setItem(backupFileHandleStorageKey, backupFileHandle, function (error) {
              if (error !== null) {
                console.error("Error saving backupFileHandle: ", error);
              }
              console.log("Saved backupFileHandle", backupFileHandle);
            });
            return callback(backupFileHandle);
          });
      }

      app.ports.uploadBackupPort.subscribe(function () {
        getBackupFileHandle(
          FilePickerType.Open,
          async function() {
            if (!(await verifyPermission(backupFileHandle))) {
              return;
            }
            var file = await backupFileHandle.getFile();
            var backupJson = JSON.parse(await file.text());
            app.ports.succeededInBackupPort.send(null);
            if (backupJson !== null && backupJson.model !== null && backupJson.simpleCharSvgs !== null) {
              localforage.setItem(simpleCharSvgsStorageKey, backupJson.simpleCharSvgs, function (error) {
                if (error !== null) {
                  console.error("Error saving simpleCharSvgs: ", error);
                }
                app.ports.gotNewSimpleCharsPort.send(backupJson.simpleCharSvgs);
                app.ports.getModelPort.send(backupJson.model);
              });
            }
          }
        )
      });

      localforage.getItem(simpleCharSvgsStorageKey, function (error, savedSimpleCharSvgs) {
        if (error !== null) {
          console.error("Error getting saved simpleCharSvgs: ", error);
        }
        // console.log("Getting saved simpleCharSvgs: ", savedSimpleCharSvgs);
        if (savedSimpleCharSvgs !== null) {
          app.ports.gotSavedSimpleCharsPort.send(savedSimpleCharSvgs);
        }
      });
    });
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
