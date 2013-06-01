(function( urlquery ){

  urlquery.pageParamMap = function() {
    return urlquery.paramMap( window.location.toString() );
  }

  urlquery.paramMap = function( url ) {
    return _.reduce( queryParams( url ), function(memo,param) {
      var elems = param.split( "=" );
      memo[elems[0]]=unescape(elems[1]);
      return memo;
    }, {} );
  }

  function queryParams( url ) {
    return queryString( url ).split("&");
  }

  function queryString( url ) {
    return url.split( "?" )[1] || "";
  }

}(window.urlquery = window.urlquery || {}))