import * as esbuild from "https://deno.land/x/esbuild@v0.18.15/mod.js";

await esbuild.build({
  entryPoints: ['pisshitballs.tsx'],
  bundle: true,
  outfile: 'cock.js',
  loader: { '.tsx': 'tsx' },
  external: ['https://esm.sh/react', 'https://esm.sh/react-dom'],
});

esbuild.stop();
