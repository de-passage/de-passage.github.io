# de-passage.github.io

Personal website at [sylvainleclercq.com](https://sylvainleclercq.com), built with PureScript and Halogen.

## Development

Use Node.js 22.5 or newer (a current LTS release is recommended). The compiler,
Spago, and esbuild are installed locally; no global PureScript tools are needed.

```sh
npm ci
npm run dev
```

Open <http://localhost:8000/index_dev.html>, or use the forwarded Coder app URL
for port 8000. The development server listens on all interfaces so it can be
accessed through that external connection. It compiles the source first, watches
`src` for PureScript and FFI changes, and reloads the browser when the bundle
changes. Compilation errors appear in the terminal. Restart the server after
changing `spago.yaml`; refresh the page after editing static assets. Set `PORT` or
`DEV_HOST` to override the defaults when needed.

```sh
npm run build  # Compile PureScript
npm test       # Check language parsing, localization, resume links, and Markdown FFI
npm run bundle # Compile and regenerate the production index.js
```

The repository serves `index.html`, `index.js`, and `assets/` directly. Commit the
regenerated `index.js` with source changes when preparing a site update. Development
output goes into the ignored `.dev/` directory. Keep both `package-lock.json` and
`spago.lock` committed for reproducible dependency versions.

## Deploying to GitHub Pages

GitHub Pages serves the repository's `master` branch from its root directory. The
production bundle is checked in as `index.js`, so publish a new version by building
that file and pushing it together with any source or asset changes:

```sh
git fetch upstream
git switch master
git pull --ff-only upstream master
npm ci
npm test
npm run bundle
git add index.js assets/ src/ package.json package-lock.json spago.yaml spago.lock CNAME
git commit -m "Deploy website update"
git push upstream master
```

Only stage the paths that changed in the update. `npm run bundle` compiles the
PureScript sources and writes the browser bundle to `index.js`; `index.html`,
`assets/`, and `CNAME` are then served directly by Pages. After the push, GitHub
Pages publishes the `master` root automatically. The custom domain is kept by the
tracked `CNAME` file, which should remain `sylvainleclercq.com`.

When working from a feature branch or pull request, run `npm run bundle` and commit
the regenerated `index.js` before merging. After the pull request is merged, the
same `master` deployment steps above publish the new version.
