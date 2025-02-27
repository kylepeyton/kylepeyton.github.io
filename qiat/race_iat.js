define(['pipAPI', 'https://kyle-peyton.com/qiat/iat10_mobile.js'], function(APIConstructor, iatExtension){
    var API = new APIConstructor();

	return iatExtension({
		category1 : {
			name : 'Black people', //Will appear in the data.
			title : {
				media : {word : 'Black people'}, //Name of the category presented in the task.
				css : {color:'#31940F','font-size':'2em'}, //Style of the category title.
				height : 4 //Used to position the "Or" in the combined block.
			}, 
			stimulusMedia : [ //Stimuli content as PIP's media objects
    		    {image : 'bf1.jpeg'}, 
    			{image : 'bf2.jpeg'}, 
    			{image : 'bf3.jpeg'}, 
    			{image : 'bm1.jpeg'}, 
    			{image : 'bm2.jpeg'}, 
    			{image : 'bm3.jpeg'}
			], 
			//Stimulus css (style)
			stimulusCss : {color:'#31940F','font-size':'1.8em'}
		},	
		category2 :	{
			name : 'White people', //Will appear in the data.
			title : {
				media : {word : 'White people'}, //Name of the category presented in the task.
				css : {color:'#31940F','font-size':'2em'}, //Style of the category title.
				height : 4 //Used to position the "Or" in the combined block.
			}, 
			stimulusMedia : [ //Stimuli content as PIP's media objects
    		    {image : 'wf1.jpeg'}, 
    			{image : 'wf2.jpeg'}, 
    			{image : 'wf3.jpeg'}, 
    			{image : 'wm1.jpeg'}, 
    			{image : 'wm2.jpeg'}, 
    			{image : 'wm3.jpeg'}			], 
			//Stimulus css
			stimulusCss : {color:'#31940F','font-size':'1.8em'}
		},	

		base_url : {//Where are your images at?
			image : 'https://kyle-peyton.com/qiat/img/'
		} 
	});
});