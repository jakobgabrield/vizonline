const { exec } = require("child_process");
const { languageToCompiler } = require('../languageMaps');

const executeProgram = async (filepath, args) => {
    let run = "";
    if (args == "") {
        // run = `cd ./functions/viz && eval $(opam config env) && ./vizOutput online_programs/${filepath} 2> errors.txt || true`;
        run = `cd ./functions/viz && eval $(opam config env) && ./vizOutput online_programs/${filepath}`;
    } else if (args == "build") {
        // run = `cd ./functions/viz && eval $(opam config env) && ./viz online_programs/${filepath} 2> errors.txt || true`
        run = `cd ./functions/viz && eval $(opam config env) && ./viz online_programs/${filepath}`;
    } else {
        run = `cd ./functions/viz && eval $(opam config env) && dune exec -- vc online_programs/${filepath} ${args}`;
    }
    return new Promise((resolve, reject) => {
        exec(
        `${run}`,
          (error, stdout, stderr) => {
              error && reject({ error, stderr });
              stderr && reject(stderr);
              resolve(stdout);
            }
        );
    });
}

module.exports = {
    executeProgram
}
