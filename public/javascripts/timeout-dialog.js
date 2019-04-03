
if(window.location.pathname !== "/disguised-remuneration/time-out") {
    $.timeoutDialog({
        timeout: 900,
        countdown: 120,
        keep_alive_url: window.location.href,
        restart_on_yes: true,
        logout_url: '/disguised-remuneration/time-out',
        keep_alive_button_text: 'Stay signed in',
        sign_out_button_text: 'Sign out'
    })};