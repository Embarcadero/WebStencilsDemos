(() => {
  'use strict'

  const getStoredTheme = () => localStorage.getItem('theme')
  const setStoredTheme = theme => localStorage.setItem('theme', theme)

  const getPreferredTheme = () => {
    const storedTheme = getStoredTheme()
    if (storedTheme) {
      return storedTheme
    }
    return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
  }

  const setTheme = theme => {
    if (theme === 'auto' && window.matchMedia('(prefers-color-scheme: dark)').matches) {
      document.documentElement.setAttribute('data-bs-theme', 'dark')
    } else {
      document.documentElement.setAttribute('data-bs-theme', theme)
    }
  }

	const updateIcon = (theme) => {
		const themeIcon = document.querySelector('#theme-icon')
		const themeToggleBtn = document.querySelector('#theme-toggle-btn')
		if (themeIcon && themeToggleBtn) {
			if (theme === 'dark') {
				themeIcon.classList.remove('bi-sun')
				themeIcon.classList.add('bi-moon-stars')
				themeToggleBtn.setAttribute('aria-label', 'Switch to light mode')
			} else {
				themeIcon.classList.remove('bi-moon-stars')
				themeIcon.classList.add('bi-sun')
				themeToggleBtn.setAttribute('aria-label', 'Switch to dark mode')
			}
		}
	}

  const toggleTheme = () => {
    const currentTheme = getStoredTheme() || getPreferredTheme()
    const newTheme = currentTheme === 'dark' ? 'light' : 'dark'
    setStoredTheme(newTheme)
    setTheme(newTheme)
    updateIcon(newTheme)
  }

  // Initialize theme immediately to avoid flickering
  const currentTheme = getPreferredTheme()
  setTheme(currentTheme)

  // Set up event listeners and update icon when DOM is ready
  const domReady = (fn) => {
    if (document.readyState === "complete" || document.readyState === "interactive") {
      setTimeout(fn, 1)
    } else {
      document.addEventListener("DOMContentLoaded", fn)
    }
  }

  domReady(() => {
    updateIcon(currentTheme)

    const themeToggleBtn = document.querySelector('#theme-toggle-btn')
    if (themeToggleBtn) {
      themeToggleBtn.addEventListener('click', toggleTheme)
    }

    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
      const storedTheme = getStoredTheme()
      if (storedTheme !== 'light' && storedTheme !== 'dark') {
        setTheme(getPreferredTheme())
      }
    })
  })
})()