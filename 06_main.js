// In this file you initialize and configure your experiment using magpieInit

$("document")
  .ready(function () {
    // prevent scrolling when space is pressed
    window.onkeydown = function (e) {
      if (e.keyCode === 32 && e.target === document.body) {
        e.preventDefault();
      }
    };

    // calls magpieInit
    // in debug mode this returns the magpie-object, which you can access in the console of your browser
    // e.g. >> window.magpie_monitor or window.magpie_monitor.findNextView()
    // in all other modes null will be returned
    window.magpie_monitor = magpieInit({
      // You have to specify all views you want to use in this experiment and the order of them
      views_seq: [
      intro,
      instructions_pretest,
      // instructions_train1_colors,
      // sentence_choice_custom,
      // instructions_train2,
      // animation_view1,
      // instructions_train3,
      // animation_view2,
      // instructions_test,
      // multiple_slider,
      slider_rating_pretest,
      // post_test,
      post_test_simple,
      thanks
    ],
      // Here, you can specify all information for the deployment
      deploy: {
        experimentID: "14",
        serverAppURL: "https://mcmpact.ikw.uni-osnabrueck.de/magpie/api/submit_experiment/",
        // Possible deployment methods are:
        // "debug" and "directLink"
        // As well as "MTurk", "MTurkSandbox" and "Prolific"
        deployMethod: "Prolific",
        contact_email: "britta.grusdt@uni-osnabrueck.de",
        prolificURL: "https://app.prolific.co/submissions/complete?cc=1A1C9196"
      },
      // Here, you can specify how the progress bar should look like
      progress_bar: {
        in: [
        // list the view-names of the views for which you want a progress bar
        animation_view1.name,
        multiple_slider.name,
        slider_rating_pretest.name
      ],
        // Possible styles are "default", "separate" and "chunks"
        style: "separate",
        width: 100
      }
    });
  });
