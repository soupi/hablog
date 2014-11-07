$(function() {
  $("#tagform").submit(submitTags);
});

function submitTags(e)
{
  var checkedValues = $('input:checkbox:checked').map(function() {
        return this.name;
  }).get();
  window.location.replace("/anime?tags=" + checkedValues);
  e.preventDefault();

}
