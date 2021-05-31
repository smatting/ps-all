nix-shell shell.nix --run "
# spago init --tag psc-0.13.8-20210226
spago install
npm install
"
