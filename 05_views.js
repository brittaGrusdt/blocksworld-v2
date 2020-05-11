// In this file you can instantiate your views
// We here first instantiate wrapping views, then the trial views

/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://magpie-ea.github.io/magpie-docs/01_designing_experiments/01_template_views/#wrapping-views

*/

// Every experiment should start with an intro view. Here you can welcome your participants and tell them what the experiment is about
const intro = magpieViews.view_generator("intro", {
  trials: 1,
  name: "intro",
  // If you use JavaScripts Template String `I am a Template String`, you can use HTML <></> and javascript ${} inside
  text: `Thank you for your participation in our study!
         Your anonymous data makes an important contribution to our understanding of human language use.
          <br />
          <br />
          Legal information:
          By answering the following questions, you are participating in a study
          being performed by scientists from the University of Osnabrueck.
          <br />
          <br />
          You must be at least 18 years old to participate.
          <br />
          <br />
          Your participation in this research is voluntary.
          You may decline to answer any or all of the following questions.
          You may decline further participation, at any time, without adverse consequences.
          <br />
          <br />
          Your anonymity is assured; the researchers who have requested your
          participation will not receive any personal information about you.
          `,
  buttonText: "begin the experiment"
});

const instructions_general = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_general",
  title: "General Instructions",
  text: `In this experiment you are shown pictures of different arrangements of
          blocks.
         <br />
         <br />
         The experiment consists of two phases, a training and a testing phase.
         In total, you will need about 15-20 minutes to finish it.
         <br/>
         <br/>
         Please note:
         <br/>
         Throughout the experiment, you may want to go into Full Screen Mode
         (usually switched on/off with F11), in order to minimize the need to
         scroll down to see all buttons.`,
  buttonText: "START"
});

// We will start with the first part of the training phase which consists
// of 8 trials. You will see block arrangements similar to those you will
// be shown later in the test phase, such that you are able to develop
// intuitions about the physical properties and get familiar with the
// stimuli.`,


// For most tasks, you need instructions views
const instructions_train2 = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train2",
  title: "Instructions Training",
  text: `We will start with the the training phase
          which consists of 11 trials. You will see block arrangements similar
          to those you will be shown later in the test phase, such that you are
          able to develop intuitions about the physical properties and get
          familiar with the stimuli.
          <br/>
          <br/>
        For each presented scene you will be asked to indicate which of the
         colored blocks you think will fall by clicking on the button with the
         respective icons.
         <br />
         A falling block is represented by a skewed rectangle
        and a resting block that <i>does not fall</i> is represented by a rectangle
        with a line below. Here is an exemplary icon for the event
        <br/>
        <i>The <b>green</b> block <b>falls</b>, but the <b>yellow</b> block
        <b>does not fall</b></i>:
        <br/>
        <br/>
        <img src='stimuli/img/icons/green.png'/>
        <img src='stimuli/img/icons/not-yellow.png' />
         <br/>
         <br/>
         A block is considered to <b><i>fall</i></b> <b>as soon as it <i>topples
         over</i> or <i>drops</i> from a platform or from another block.</b>
         <br/>
         The colored blocks represent common toy blocks, they <b>do not have</b> any
         <i>special or unexpected</i> properties and they are only distinguishable by
        their color.
             <br />
             <br />
         After you selected one of the four buttons (whose border will turn green),
         you may click on RUN to see what actually happens. If you were wrong,
         the selected button will turn red and the correct one will turn light
         green.
         Then, you can proceed to the next trial.`,
  buttonText: "CONTINUE"
});

const instructions_train3 = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train3",
  title: "Instructions Training",
  text: `Great - there is only one trial left in the training phase to which we
  will proceed now.
    <br />
  In this trial, we ask you to indicate <b>how likely</b> you think certain
  blocks <b>will or will not fall</b> by moving the corresponding sliders.
   <br />
  The larger your belief is that the event you are asked for <b>will</b> occur,
  the more you should position the corresponding slider towards its
  right end (<i>certainly</i>/100%).
  <br />
  The larger your belief is that the event <b>will not</b> occur, the more you
  should position the corresponding slider towards its left (<i>impossible</i>/0%).
  Here is an example:
  <br />
  <br />
  <input type='range' id=ex_slider class='magpie-response-slider replied' min='0' max='100' value='40' oninput='ex_slider.value + "%"'/>
  <output name=ex_slider_out id=out_ex class="thick">40%</output>
  <script>document.getElementById("ex_slider").disabled=true;</script>
  <br/>
  <br/>
  If you positioned the slider like or close to this at 40%, you indicate that you
  are quite <b>undecided</b> whether or not the event will occur, but you judge
  it <i>a bit <b>more</b> likely that it will <b>not</b></i> occur.
  <br />
  The circle on the sliders will turn green after you moved it and clicked to
  fix its new position, as is shown on the example slider above.
    <br />
    <br />
  After you have provided all four estimates (the circles of all four sliders
  have to be green), you will be able to run the animation and after that,
  proceed to the test phase of the experiment.
  `
});

const instructions_test = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_test",
  title: "Instructions Test Phase",
  text: `Great - you've now completed the training phase and we'll continue with
          the test phase next.
          <br />
          Again, you will be shown scenes of different block
          arrangements.
          <br />
          As in the previous trial, we will ask you to
          indicate <b>how likely</b> you think certain blocks <b>will or will
          not fall</b>. The only difference is, that now you will <i>not</i> get
          feedback about what will actually happen since you will be shown
          static pictures only.
          <br />
          After you have provided all four estimates (the circles of all four
          sliders have to be green), you will be able proceed to the next trial.
            <br />
            <br />
          Please note:
          <br />
          You may have wondered whether the probabilities that you assign to the
          four described events must sum up to 100%. Since we are interested in how
          you rate the four events relative to each other, your estimates
          <b>may</b>, but <b>do not have to</b> <i>sum to 100%</i>.
          <br />
          <br />
          Two more things to keep in mind:
            <br/>
          1. A block is considered to <b><i>fall</i> as soon as it <i>drops</i> from a
          platform or from another block or simply <i>topples over</i></b> -
          that is, a block does not necessarily need to fall to the ground in
          order to count as <i>falling</i>.
          <br/>
          3. The colored blocks all have <b>the same properties</b>, they are only
          distinguishable by their color.
            </br>
            </br>
          We will now start the experiment. There are <b>20</b> scenes in total.`,
  buttonText: "start experiment"
});


// In the post test questionnaire you can ask your participants addtional questions
const post_test = magpieViews.view_generator("post_test", {
  trials: 1,
  name: "post_test",
  title: "Additional information",
  text: "Answering the following questions is optional, but your answers will help us analyze our results.",

  // You can change much of what appears here, e.g., to present it in a different language, as follows:
  // buttonText: 'Weiter',
  // age_question: 'Alter',
  // gender_question: 'Geschlecht',
  // gender_male: 'männlich',
  // gender_female: 'weiblich',
  // gender_other: 'divers',
  // edu_question: 'Höchster Bildungsabschluss',
  // edu_graduated_high_school: 'Abitur',
  // edu_graduated_college: 'Hochschulabschluss',
  // edu_higher_degree: 'Universitärer Abschluss',
  // languages_question: 'Muttersprache',
  // languages_more: '(in der Regel die Sprache, die Sie als Kind zu Hause gesprochen haben)',
  comments_question: 'Further comments <br/><small>(was there anything about the different stimuli you realized in particular?)</small>'
}, {
  answer_container_generator: custom_posttest_generator.answer_container_gen,
  handle_response_function: custom_posttest_generator.handle_response_function
});


const post_test_simple = magpieViews.view_generator("post_test", {
  trials: 1,
  name: "post_test",
  title: "Additional information",
  text: "Answering the following questions is optional, but your answers will help us analyze our results.",
  comments_question: 'Further comments'
});

const instructions_pretest = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_pretest",
  title: "Instructions Test Phase",
  text: `In this short experiment, we ask you to indicate <b>how likely</b> you think
  a shown toy block <b>will or will not fall</b> by moving a slider.
   <br />
  The larger your belief is that the block <b>will</b> fall,
  the more you should position the slider towards its right end (<i>certainly</i>/100%).
  <br />
  The larger your belief is that it <b>will not</b> fall, the more you
  should position the slider towards its left (<i>impossible</i>/0%).
  <br/>
  If you are quite <b>undecided</b> whether or not the block will fall, but you judge
  it <i>a bit <b>more</b> likely that it <b>won't</b></i> fall, you should for instance move the slider a bit to the left.
    <br />
    <br />
  After you have given your estimate by changing the position of the slider, you
  will see a button saying NEXT which will bring you to the next trial.
  `
});

const instructions_train_pretest = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train_pretest",
  title: "Instructions Train Phase",
  text: `We will now start with the the training phase
          which consists of 8 trials. You will see block arrangements such that
          you are able to develop intuitions about the physical properties and get
          familiar with the stimuli.
          <br/>
          <br/>
        For each presented scene you will be asked to indicate which of the
         colored blocks you think will fall by clicking on the button with the
         respective icons.
         A falling block is represented by a skewed rectangle
        and a resting block that <i>does not fall</i> is represented by a rectangle
        with a line below. Here is an exemplary icon for the event
        <br/>
        <i>The <b>green</b> block <b>falls</b>, but the <b>yellow</b> block
        <b>does not fall</b></i>:
        <br/>
        <br/>
        <img src='stimuli/img/icons/green.png'/>
        <img src='stimuli/img/icons/not-yellow.png' />
         <br/>
         <br/>
         A block is considered to <b><i>fall</i></b> <b>as soon as it <i>topples
         over</i> or <i>drops</i> from a platform or from another block.</b>
         <br/>
         The colored blocks represent common toy blocks, they do not have any
         special or unexpected properties and they are only distinguishable by
         their color.
             <br />
             <br />
         After you selected one of the four buttons (whose border will turn green),
         you may click on RUN to see what actually happens. If you were wrong,
         the selected button will turn red and the correct one will turn light
         green.
         Then, you can proceed to the next trial.`,
  buttonText: "CONTINUE"
});



// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = magpieViews.view_generator("thanks", {
  trials: 1,
  name: "thanks",
  title: "Thank you very much for taking part in this experiment!",
  prolificConfirmText: "Press the button"
});

const sentence_choice_custom = magpieViews.view_generator("sentence_choice", {
  trials: color_vision_test.length,
  name: "color-vision",
  data: _.shuffle(color_vision_test)
}, {
  stimulus_container_generator: function (config, CT) {
    return `<div class='magpie-view'>
    <h1 class='magpie-view-title'>${config.title}</h1>
    <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
                        <div class='magpie-view-stimulus-container'>
                          <img src="${config.data[CT].picture1}" class = "img" >
                          <img src="${config.data[CT].picture2}" class = "img">
                        </div>
                      </div>`;
  }
});

const slider_rating_pretest = magpieViews.view_generator("slider_rating", {
  trials: pretest_trials.length,
  name: "pretest",
  data: pretest_trials
},
{
  stimulus_container_generator: function (config, CT) {
      return `<div class='magpie-view'>
        <h1 class='magpie-view-title'>${config.title}</h1>
        <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
        <div class='stimulus' id='stimulus-pic'>
          <img src=${config.data[CT].picture}>
        </div>
      </div>`;
  }
});

// experimental phase trials
const multiple_slider = magpieViews.view_generator(
  "slider_rating", {
    // This will use all trials specified in `data`, you can use a smaller value
    // (for testing), but not a larger value
    trials: TEST_TRIALS.length,
    // trials: 4,
    // name should be identical to the variable name
    name: "multiple_slider",
    data: TEST_TRIALS
  },
  // you can add custom functions at different stages through a view's life cycle
  {
    stimulus_container_generator: multi_slider_generator.stimulus_container_gen,
    answer_container_generator: multi_slider_generator.answer_container_gen,
    handle_response_function: multi_slider_generator.handle_response_function
  }
);
