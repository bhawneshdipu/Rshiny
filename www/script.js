$(document).ready(function(){
  $('body > div > header > nav > div > ul').attr("class","sidebar-menu nav navbar-nav mynavbar");
  $('body > div > header > nav > div > ul > li').removeClass("dropdown").addClass("navbar-li");

  $('#sidebarItemExpanded > ul > li:not(.active)').css("display",'none');
  $('#sidebarItemExpanded > ul > li.active').css("display",'');
  
});
$(document).on("click",function(){
  $('#sidebarItemExpanded > ul > li:not(.active)').css("display",'none');
  $('#sidebarItemExpanded > ul > li.active').css("display",'');
  
});
$(document).on("click",'.mynavbar-toggle',function(){
  console.log("clicked on mynavbar-toggle");
  $('.mynavbar').toggle();
});


$(document).on("click",'.sidebar-toggle',function(){
  $('.mynavbar').show();
});
