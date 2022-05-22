import fs from "fs";
import path from "path";
import url from "url";

const __dirname = url.fileURLToPath(new URL(".", import.meta.url));

async function getMatchingModules(pattern) {
  const directories = await fs.promises.readdir(path.join(__dirname, ".."));
  const modules = await Promise.all(
    directories
      .filter((directory) => new RegExp(pattern).test(directory))
      .map(async (name) => {
        const module = await import(
          path.join(__dirname, "..", name, "index.js")
        );
        return module && typeof module.spec !== "undefined"
          ? module.spec
          : null;
      })
  );
  return modules.filter((x) => x);
}

export function getSpecs(pattern) {
  return () => getMatchingModules(pattern);
}
