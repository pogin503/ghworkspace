'use strict';

var $ = require('jQuery');

$(function() {
  $('h1').css('color', 'red');
})

// var $ = require('jQuery')
// $('#NWrelart\\:Body > h2').each(function(i,v){console.log($(v).text())})

// array merge
var arr1 = [1,2,3]
var arr2 = [4,5,6]
var arr3 = arr2.concat(arr1);

var arr = [];
$('button[tabindex],input[tabindex],textarea[tabindex]').each(function(){
  arr.push({
    // id
    name: $(this).attr('id'),
    // tag name
    nodeName: $(this).nodeName,
    // get tab index
    value: $(this).attr('tabindex')
  });
});

arr.sort(
  function(a,b){
    if (a['value'] > b['value']) return -1;
    if (a['value'] < b['value']) return 1;
    return 0;
  });

