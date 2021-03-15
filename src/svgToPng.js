/**
 * Source: https://ramblings.mcpher.com/gassnippets2/converting-svg-to-png-with-javascript/
 * converts an svg string to base64 png using the domUrl
 * @param {string} svgText the svgtext
 * @param {number} [margin=0] the width of the border - the image size will be height+margin by width+margin
 * @return {Promise} a promise to the bas64 png image
 */
var svgToPng = function (svgText, margin) {
    // convert an svg text to png using the browser
    return new Promise(function (resolve, reject) {
        try {
            // can use the domUrl function from the browser
            var domUrl = window.URL || window.webkitURL || window;
            if (!domUrl) {
                throw new Error("(browser doesnt support this)")
            }

            // figure out the height and width from svg text
            var match = svgText.match(/height=\"(\d+)/m);
            var height = match && match[1] ? parseInt(match[1], 10) : 200;
            var match = svgText.match(/width=\"(\d+)/m);
            var width = match && match[1] ? parseInt(match[1], 10) : 200;
            margin = margin || 0;

            // it needs a namespace
            if (!svgText.match(/xmlns=\"/mi)) {
                svgText = svgText.replace('<svg ', '<svg xmlns="http://www.w3.org/2000/svg" ');
            }

            // create a canvas element to pass through
            var canvas = document.createElement("canvas");
            canvas.width = height + margin * 2;
            canvas.height = width + margin * 2;
            var ctx = canvas.getContext("2d");


            // make a blob from the svg
            var svg = new Blob([svgText], {
                type: "image/svg+xml;charset=utf-8"
            });

            // create a dom object for that image
            var url = domUrl.createObjectURL(svg);

            // create a new image to hold it the converted type
            var myImage = new Image();

            // when the image is loaded we can get it as base64 url
            myImage.onload = function () {
                // draw it to the canvas
                ctx.drawImage(myImage, margin, margin);
                // we don't need the original any more
                domUrl.revokeObjectURL(url);
                // now we can resolve the promise, passing the base64 url
                resolve(canvas.toDataURL());
            };
            // load the image
            myImage.src = url;
        } catch (err) {
            reject('failed to convert svg to png ' + err);
        }
    });
};

export { svgToPng };