/* global angular */
(function(angular) {

    angular
        .module('viewFactories')
        .factory('&1&3', ['$q', function($q) {

            function &2&3() {
            }

            &2&3.prototype = {
                /*  The resolve method could return arbitrary data, 
                    which will be available in the "viewShowHandler" and "viewHideHandler" handler as the customData argument */
                onInit: function($stateParams) {
                    return $q(function(resolve, reject) {
                        resolve({});
                    });
                },
                /* "customData" is the data return by the viewInitHandler handler*/
                onShow: function($scope, customData) {

                },
                /* "customData" is the data return by the viewInitHandler handler*/
                onHide: function(customData) {

                },
                /* Kendo event object*/
                onRowSelect: function(e) {

                }
            };

            return new &2&3();
        }]);

})(angular);
