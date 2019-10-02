/* global $ */
/* global jQuery */
/* global GOVUK */

$(document).ready(function () {
    // Turn off jQuery animation
    jQuery.fx.off = true;
    document.getElementById("error-summary-heading").focus();
    $('input.volume').keyup(function(event) {
        // format number
        $(this).val(function(index, value) {
            return value
                .replace(/\D/g, "")
                .replace(/\B(?=(\d{3})+(?!\d))/g, ",");
        });
    });
});

function removeSpecialChars( myid ) {
    return myid.replace( /(:|\.|\[|\]|,|=|@)/g, "\\$1" );
}

window.onload = function () {
    function gaWithCallback(send, event, category, action, label, callback) {
        ga(send, event, category, action, label, {
            hitCallback: gaCallback
        });
        var gaCallbackCalled = false;
        setTimeout(gaCallback, 5000);

        function gaCallback() {
            if(!gaCallbackCalled) {
                callback();
                gaCallbackCalled = true;
            }
        }
    }
};