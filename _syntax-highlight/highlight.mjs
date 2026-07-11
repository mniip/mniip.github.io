import { readdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { createStarryNight } from "@wooorm/starry-night";
import { toHtml } from "hast-util-to-html";

import sourceHaskell from "@wooorm/starry-night/source.haskell";
import sourceLean4 from "@wooorm/starry-night/source.lean4";

const destDir = process.argv[2];

const html_entity_decode = (s) =>
  s
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&amp;/g, "&");

const starryNight = await createStarryNight([
  sourceHaskell,
  sourceLean4,
]);

const CODE_BLOCK_RE =
  /<pre><code class="language-([\w.+#-]+)">([\s\S]*?)<\/code><\/pre>/g;

const files = (await readdir(destDir, { recursive: true }))
  .filter((f) => f.endsWith(".html"))
  .map((f) => path.join(destDir, f));

for (const file of files) {
  let changed = false;
  const html = (await readFile(file, "utf8")).replace(
    CODE_BLOCK_RE,
    (whole, lang, body) => {
      if (body.includes("<")) return whole; // already highlighted
      const scope = starryNight.flagToScope(lang);
      if (!scope) return whole; // unknown language
      changed = true;
      const new_body = toHtml(
        starryNight.highlight(
          html_entity_decode(body).replace(/\n$/, ""),
          scope
        )
      );
      return `<pre><code class="language-${lang}">${new_body}</code></pre>`;
    },
  );
  if (changed) await writeFile(file, html);
}
