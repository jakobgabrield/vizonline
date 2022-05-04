const { exec } = require("child_process");
const { languageToCompiler } = require('../languageMaps');

const executeProgram = async (filepath, args) => {
    let run = "";
    if (args == "") {
        run = `cd ./functions/viz && ./vizDocker online_programs/${filepath} 2> errors.txt || true`;
    } else {
        run = `cd ./functions/viz && eval $(opam config env) && dune exec -- vc online_programs/${filepath} ${args}`;
    }
    return new Promise((resolve, reject) => {
        exec(
        //   `cd ./functions/viz && dune exec -- vc ${filepath} ${args}`,
        `${run}`,
        // `cd ./functions/viz && eval $(opam env) && dune exec -- vc online_programs/${filepath} ${args}`,
        // `cd ./functions/viz && dune exec -- vc online_programs/${filepath} ${args}`,
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