// Below is heavily lifted from the following link:
// https://book.javascript-for-r.com/shiny-cookies.html 

// Function to pass cookie info to Shiny
function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}

// Function to receive set cookie command info from Shiny - needed to record 
// user's consent choice
Shiny.addCustomMessageHandler('cookie-set', function(msg){
  Cookies.set(msg.name, msg.value, { expires: 30 });
  getCookies();
})

// Function to receive remove cookie command info from Shiny - needed to cancel 
// user's consent choice
Shiny.addCustomMessageHandler('cookie-remove', function(msg){
  Cookies.remove(msg.name);
  getCookies();
})

$(document).on('shiny:connected', function(ev){
  getCookies();
})

// Function to set analytics consent dynamically - needed to fulfil user's 
// consent choice
Shiny.addCustomMessageHandler('analytics-consent', function(msg){
  gtag('consent', 'update', {
    'analytics_storage': msg.value
  });
})


