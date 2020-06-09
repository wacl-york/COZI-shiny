// Modified version of assets/spinner.js from shinycssloaders
// Stops spinner from hiding before plots are ready
(function() {
var output_states = [];

function escapeSelector(s) {
    return s.replace(/([!"#$%&'()*+,-./:;<=>?@\[\\\]^`{|}~])/g, "\\$1");
}

function show_spinner(id) {
    var selector = "#"+escapeSelector(id);
    $(selector).siblings(".load-container, .shiny-spinner-placeholder").removeClass('shiny-spinner-hidden');
    $(selector).siblings(".load-container").siblings('.shiny-bound-output, .shiny-output-error').css('visibility', 'hidden');
    // if there is a proxy div, hide the previous output
    $(selector).siblings(".shiny-spinner-placeholder").siblings('.shiny-bound-output, .shiny-output-error').addClass('shiny-spinner-hidden');
}

function hide_spinner(id) {
    /* 
    
    Have modified this function from the original.
    
    Rather than blanketedly hiding the spinner,
    the spinner is now hidden for every id *except* .plotui
    This is because .plotui makes divs to hold the plots in before 
    the plot pngs are made. 
    
    Instead, when the first png plot is ready - indicated by an id that contains 'out',
    the .plotui spinner is finally hidden.
    
    If the original code is used, the spinner is hidden as soon as the first of these
    inner divs is ready, which can be far before the plot pngs is ready.
    
    */
  
    var selector = "#"+escapeSelector(id);
    $(selector).siblings(".load-container").siblings('.shiny-bound-output').css('visibility', 'visible');
    // if there is a proxy div, show the previous output in case it was hidden
    $(selector).siblings(".shiny-spinner-placeholder").siblings('.shiny-bound-output').removeClass('shiny-spinner-hidden');
    
    // Don't hide the spinner for .plotui, this is just placeholder divs being ready
    if (id !== "plotui") {
      $(selector).siblings(".load-container, .shiny-spinner-placeholder").addClass('shiny-spinner-hidden');
    }
    // When have actual plots ready then finally hide the spinner
    if (id.includes("out")) {
      var selector2 = "#"+escapeSelector("plotui");
      $(selector2).siblings(".load-container, .shiny-spinner-placeholder").addClass('shiny-spinner-hidden');
    }
}

function update_spinner(id) {
  if (!(id in output_states)) {
    show_spinner(id);
  }
  if (output_states[id] <= 0) {
    show_spinner(id);
  } else {
    hide_spinner(id);
  }
}

$(document).on('shiny:bound', function(event){ 
  /* if not bound before, then set the value to 0 */
  if (!(event.target.id in output_states)) {
    output_states[event.target.id] = 0;
  }
  update_spinner(event.target.id);
});

/* When recalculating starts, show the spinner container & hide the output */
$(document).on('shiny:outputinvalidated', function(event) {
  output_states[event.target.id] = 0;
  update_spinner(event.target.id);
});

/* When new value or error comes in, hide spinner container (if any) & show the output */
$(document).on('shiny:value shiny:error', function(event) {
    var id = event.target.id;
    if (id === undefined) {
      return;
    }
    output_states[id] = 1;
    update_spinner(id);
});
}());
