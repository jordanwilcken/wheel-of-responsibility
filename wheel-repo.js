var wheelApp = wheelApp || { };

wheelApp.wheelRepo = (function() {
    return {
        generateId: generateId,
        save: save,
        getAll: getAll
    };

    function getJobWheels() {
        var wheelData = localStorage.getItem("jobWheels");
        return wheelData
            ? JSON.parse(wheelData)
            : [ ];
    }

    function generateId() {
        var
            largestId,
            jobWheels = getJobWheels();


        largestId = jobWheels.reduce(function(largest, current) {
            return current.id > largest
                ? current.id
                : largest
        }, 0);

        return largestId + 1;
    }

    function save(newWheel) {
        var myWheels = getJobWheels();
        myWheels.push(newWheel);
        localStorage.setItem("jobWheels", JSON.stringify(myWheels));
    }

    function getAll() {
        return getJobWheels();
    };
}());
