"use strict";

const ReactDOM = require("react-dom");

exports.renderConcurrentMode = (jsx) => (element) => () => {
  return ReactDOM.createRoot(element).render(jsx);
};
