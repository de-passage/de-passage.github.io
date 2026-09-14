# de-passage.github.io

Personal website at [sylvainleclercq.com](https://sylvainleclercq.com), built with PureScript and Halogen.

## Development

Use Node.js 22.5 or newer (a current LTS release is recommended). The compiler,
Spago, and esbuild are installed locally; no global PureScript tools are needed.

```sh
npm ci
npm run dev
```

Open <http://127.0.0.1:8000/index_dev.html>. The development server compiles the
source first, watches `src` for PureScript and FFI changes, and reloads the browser
when the bundle changes. Compilation errors appear in the terminal. Restart the
server after changing `spago.yaml`; refresh the page after editing static assets.

```sh
npm run build  # Compile PureScript
npm test       # Check language parsing, localization, resume links, and Markdown FFI
npm run bundle # Compile and regenerate the production index.js
```

The repository serves `index.html`, `index.js`, and `assets/` directly. Commit the
regenerated `index.js` with source changes when preparing a site update. Development
output goes into the ignored `.dev/` directory. Keep both `package-lock.json` and
`spago.lock` committed for reproducible dependency versions.

## Migration

The previous Dhall package set, `psc-0.13.6-20200331`, targeted PureScript 0.13.6
and selected Halogen 5.0.0-rc.7. The project now uses PureScript **0.15.16**,
Halogen **7.0.0**, Spago **1.0.4**, and Registry package set **81.1.0**. The set's
0.15.15 compiler baseline is compatible with the 0.15.16 compiler.

Changes follow the upstream migration instructions:

- [PureScript 0.14 guide](https://github.com/purescript/documentation/blob/master/migration-guides/0.14-Migration-Guide.md): replace `SProxy` with `Type.Proxy`.
- [PureScript 0.15 guide](https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md): convert CommonJS FFI and application entry points to ES modules.
- [Halogen 6 guide](https://purescript-halogen.github.io/purescript-halogen/changelog/v6.html) and [Halogen 7 release](https://github.com/purescript-halogen/purescript-halogen/releases/tag/v7.0.0): remove the component surface parameter, return actions directly from event handlers, and replace `HP.id_` with `HP.id`.
- [Spago migration guidance](https://github.com/purescript/spago#migrate-from-spagodhall-to-spagoyaml): replace Dhall configuration with `spago.yaml` and a Registry package set.
- [Affjax changelog](https://github.com/purescript-contrib/purescript-affjax/blob/main/CHANGELOG.md): use the browser driver from `affjax-web`. Parsing imports and Argonaut decode errors also use their current APIs.

`halogen-bootstrap4` has no PureScript 0.15 release, so `src/Bootstrap.purs`
defines just the class names used here, preserving Bootstrap 4 styling and behavior.
The unused `halogen-svg` dependency, formerly pinned to `master`, is removed.
esbuild replaces the old Parcel setup and bundles the installed Marked package
through its named ES module export.
