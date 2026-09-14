import assert from "node:assert/strict";
import { test } from "node:test";
import { marked, setHTML } from "../src/External/Marked.js";

test("Markdown FFI returns an effect that renders rich text", () => {
  const render = marked('**Hello** [website](https://example.com) -- "welcome"');
  assert.equal(typeof render, "function");
  const html = render();
  assert.match(html, /<strong>Hello<\/strong>/);
  assert.match(html, /<a href="https:\/\/example.com">website<\/a>/);
  assert.match(html, /– “welcome”/);
});

test("DOM FFI applies content only when its effect runs", () => {
  const element = { innerHTML: "old content" };
  const update = setHTML(element)(marked("*New content*")());
  assert.equal(element.innerHTML, "old content");
  update();
  assert.equal(element.innerHTML, "<p><em>New content</em></p>\n");
});
