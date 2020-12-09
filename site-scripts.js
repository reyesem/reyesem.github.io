// Toggle the drop-down navigation between visible and invisible;
function toggleNavigation() {
  document.querySelector("#menu-icon").classList.toggle("change");
  document.querySelector("#small-menu").classList.toggle("navbar-hidden");
  document.querySelector("#small-menu").classList.toggle("navbar-visible");
}


// Make active tab visible;
function displayTab(evt, tabName) {
  var i;
  
  // non-active tabs are invisible, non-active buttons are normal;
  var tabs = document.getElementsByClassName("tab");
  var tabbtns = document.getElementsByClassName("tab-button");
  for (i = 0; i < tabs.length; i++) {
    tabs[i].style.display = "none";
    tabbtns[i].className = tabbtns[i].className.replace(" tab-button-active", "");
  }
  
  // active tabs are visible, active button is darker;
  document.getElementById(tabName).style.display = "block";
  evt.currentTarget.className += " tab-button-active";
}