import { defineConfig } from 'vite'
import pkg from './package.json'

export default defineConfig({
  base: 'https://gen.glitchmeme.wtf/',
  build: {
    outDir: 'dist',
    target: 'esnext',
    assetsDir: 'assets',
    emptyOutDir: true,
    sourcemap: true
  },
  define: {
    'import.meta.env.VERSION': JSON.stringify(pkg.version)
  }
}) 
