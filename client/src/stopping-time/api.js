
exports.getApiOptimalstrategy = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/api/optimal-strategy'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'GET'
    });
}

