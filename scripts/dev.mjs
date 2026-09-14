import { spawn } from "node:child_process";
import { watch } from "node:fs";
import http from "node:http";
import { context } from "esbuild";

// Keep compilation serialized: edits during a build trigger one more build.
let pending = false;
let building = false;
let compiler;
const compile = () => new Promise((resolve) => {
  compiler = spawn("spago", ["build"], { stdio: "inherit" });
  compiler.on("error", (error) => {
    console.error(error);
    resolve(false);
  });
  compiler.on("exit", (code) => resolve(code === 0));
});

if (!await compile()) process.exit(1);

const bundle = await context({
  entryPoints: ["index_dev.js"],
  outfile: ".dev/index.js",
  bundle: true,
  sourcemap: true,
  platform: "browser",
  banner: {
    js: 'new EventSource("/esbuild").addEventListener("change", () => location.reload());',
  },
});
await bundle.rebuild();
const host = process.env.DEV_HOST ?? "0.0.0.0";
const port = Number(process.env.PORT ?? 8000);
// esbuild intentionally rejects unknown Host headers to prevent DNS rebinding.
// Coder supplies its public hostname, so put a thin proxy in front of esbuild
// and replace only the internal upstream Host header.
const server = await bundle.serve({ servedir: ".", host: "127.0.0.1", port: 0 });
const proxy = http.createServer((request, response) => {
  const upstream = http.request({
    hostname: "127.0.0.1",
    port: server.port,
    path: request.url,
    method: request.method,
    headers: {
      ...request.headers,
      host: `127.0.0.1:${server.port}`,
    },
  }, (upstreamResponse) => {
    response.writeHead(upstreamResponse.statusCode ?? 502, upstreamResponse.headers);
    upstreamResponse.pipe(response);
  });

  upstream.on("error", (error) => {
    console.error(error);
    if (!response.headersSent) response.writeHead(502);
    response.end("Development server unavailable");
  });
  request.pipe(upstream);
});
await new Promise((resolve, reject) => {
  proxy.once("error", reject);
  proxy.listen(port, host, resolve);
});
console.log(`Development site listening on ${host}:${port}/index_dev.html`);

async function rebuild() {
  pending = true;
  if (building) return;
  building = true;
  try {
    while (pending) {
      pending = false;
      if (await compile()) await bundle.rebuild();
    }
  } finally {
    building = false;
  }
}

const watcher = watch("src", { recursive: true }, (_event, filename) => {
  if (filename && /\.(purs|js)$/.test(filename)) void rebuild();
});

async function stop() {
  watcher.close();
  compiler?.kill();
  proxy.close();
  await bundle.dispose();
  process.exit(0);
}
process.once("SIGINT", stop);
process.once("SIGTERM", stop);
