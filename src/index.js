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

      app.ports.getModelPort.send(savedModelJson);

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
              var numberOfNewCharsAdded = 0;
              Array.from(files).forEach(function (file) {
                var reader = new FileReader();
                reader.addEventListener('load', function (event) {
                  var char = file.name.slice(0, -(".svg".length));
                  simpleCharSvgs[char] = event.target.result;
                  numberOfNewCharsAdded += 1;
                  if (numberOfNewCharsAdded === files.length) {
                    app.ports.gotNewSimpleCharsPort.send(simpleCharSvgs);
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

      localforage.getItem(simpleCharSvgsStorageKey, function (error, savedSimpleCharSvgs) {
        if (error !== null) {
          console.error("Error getting saved simpleCharSvgs: ", error);
        }
        if (savedSimpleCharSvgs == null || Object.keys(savedSimpleCharSvgs).length === 0) {
          var defaultChars = "上下不儿几卜口犬車門".split("");
          var simpleCharSvgs = {};
          defaultChars.forEach(function (char) {
            fetch(`default-simple-char-svgs/${char}.svg`)
              .then(function (r) {
                return r.text()
              })
              .then(function (svg) {
                // console.log("Loaded default SimpleCharSvg " + char);
                simpleCharSvgs[char] = svg;
                if (Object.keys(simpleCharSvgs).length === defaultChars.length) {
                  app.ports.gotNewSimpleCharsPort.send(simpleCharSvgs);
                  localforage.setItem(simpleCharSvgsStorageKey, simpleCharSvgs, function (error) {
                    if (error !== null) {
                      console.error("Error saving simpleCharSvgs: ", error);
                    }
                  });
                }
              });
          });
        }
        // console.log("Getting saved simpleCharSvgs: ", savedSimpleCharSvgs);
        app.ports.gotSavedSimpleCharsPort.send(savedSimpleCharSvgs);
      });
    });
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
