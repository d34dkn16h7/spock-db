GET
	/get/{Int} -- return json ( s = done)

	/getTop/ {easy | medium | hard} -- return json ( not done)

POST
	/addPlayer -- return json ( s = done,u = done )
		"name"

	/newName -- retrun json ( s = done,u = done )
		id -- Int id
		"name" -- new name

	/pScore/ {easy | medium | hard} -- json succes ( s = done,u = done )
		id -- Int id
		nScore -- Int score

id = "pID"