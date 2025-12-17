/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: 'class', // Force class-based dark mode (light mode by default)
  content: [
    "./templates/**/*.html",
    // Add any other paths where you use Tailwind classes
  ],
  theme: {
    extend: {
      // Customize your theme here if needed
      // colors: { ... },
      // spacing: { ... },
    },
  },
  plugins: [
    // Add Tailwind plugins here if needed
    // Most plugins require Node.js, so check compatibility
  ],
  // Safelist for dynamically generated classes
  safelist: [
    // Add patterns for classes generated in Delphi code
    // Example: 'bg-red-500', 'text-center', etc.
  ],
}



