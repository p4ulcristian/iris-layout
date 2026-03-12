import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  server: {
    allowedHosts: ['iris-os.irisdoes.work', '5173.irisdoes.work'],
  },
})
