import fs from "fs";
import path from "path";

if (typeof require !== "function") {
  throw new Error(
    "Sorry, purescript-spec-discovery only supports NodeJS environments!"
  );
}

function getMatchingModules(pattern) {
  const directories = fs.readdirSync(path.join(__dirname, ".."));
  return directories
    .filter((directory) => new RegExp(pattern).test(directory))
    .map((name) => {
      const module = require(path.join(__dirname, "..", name));
      return module && typeof module.spec !== "undefined" ? module.spec : null;
    })
    .filter((x) => x);
}

export function getSpecs(pattern) {
  return () => getMatchingModules(pattern);
}
