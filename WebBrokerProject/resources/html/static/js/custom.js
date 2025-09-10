document.addEventListener('DOMContentLoaded', (event) => {
    const copyButtons = document.querySelectorAll('.copy-btn');
    copyButtons.forEach(button => {
        button.addEventListener('click', () => {
            const codeElement = button.closest('.card-body').querySelector('code');
            const codeText = codeElement.textContent;
            
            navigator.clipboard.writeText(codeText.trim()).then(() => {
                const originalHTML = button.innerHTML;
                button.textContent = 'Copied!';
                setTimeout(() => {
                    button.innerHTML = originalHTML;
                }, 2000);
            }).catch(err => {
                console.error('Failed to copy: ', err);
            });
        });
    });
});