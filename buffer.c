/**********************************************************************************************************************
File name:			buffer.c
Compiler:			MS Visual Studio 2019
Version: 1.20.2
Author:				Vidhi Patel
Student Number:     040955982
Date:				1 June 2020
Course:				CST 8152 - Compilers, Lab Section: 11
Assignment:			01
Professor:			Svillen Ranev
Purpose:			To create a buffer that can run on three different modes Fixed, Additive and Multiplicative.
					Fixed - The capacity of the buffer is pre defined and fixed which means
							it cannot be changed once the program starts.
					Additive - The capacity is increased by adding pre defined incremental value
							until the capacity limit is reached.
					Mulitplicative - The capacity is increased by performing some calculations
							in which incremental value is multiplied by the space between the
							current capacity and capacity limit, then divided by
							100 and added to the current capacity;
Function list:		b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_addcoffset(), b_capacity(),
					b_markc(), b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(),
					b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(),
					b_rewind(),b_location()
***********************************************************************************************************************/


#include "buffer.h"

/************************************************************************************************************************
Purpose:			to allocate dynamic memory based on given parameters
Author:				Vidhi Patel
History/Versions:	1.0 - Jun 3, 2020
Called Function:	calloc(), malloc(), free()
Parameters:			init_capacity - should be smaller than SHRT_MAX-1 and not less than 0,
					inc_factor - Should be between 0 and 255 for additive mode or
								between 0 and 100 for mulitplicative mode,
					o_modes - should be equal to 'f', 'a' or 'm' for Fixed, Additive
								and Multiplicative modes respectively.
Return Value:		returns pointer to the buffer structure
Algorithm:			first it uses calloc function to allocate memory for single buffer,
					then it uses malloc function to allocate memory for cb_head with init_capacity,
					if mode is anything but 'f', 'a' or 'm' it returns NULL.
					if mode is 'f' then it assigns FIXED_MODE (0) to the buffer.
					if mode is 'a' then it assigns ADDITIVE_MODE (1) and passes value of inc_factor to the buffer.
					if mode is 'm' then it assigns MULTIPLICATIVE_MODE (-1) and passes value of inc_factor to the buffer.
					then passes capacity to the buffer.
					And then sets flags to default before returning pointer to buffer structure.
************************************************************************************************************************/


Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {

	pBuffer pBD; /* pointer to buffer descriptor*/

	/* Allocate memory to buffer struct*/
	pBD = (Buffer*)calloc(1, sizeof(Buffer));

	if (pBD == NULL) {
		return NULL;
	}

	/*	Check the range of capacity*/

	if (init_capacity < 0 || init_capacity > MAX_CAP) {
		free(pBD);
		pBD = NULL;
		return NULL;
	}
	/*	Allocate memory for one dynamic character	*/

	if (init_capacity == 0) {

		pBD->cb_head = (char*)malloc(sizeof(char) * DEFAULT_INIT_CAPACITY);

		if (pBD->cb_head == NULL) {

			free(pBD);
			pBD = NULL;
			return NULL;
		}

		if (o_mode == 'a') {
			pBD->inc_factor = INC_FACTOR_A_M;
			pBD->mode = MODE_ADD;
		}

		else if (o_mode == 'm') {
			pBD->inc_factor = INC_FACTOR_A_M;
			pBD->mode = MODE_MULTI;
		}


		else if (o_mode == 'f') {
			pBD->inc_factor = 0;
			pBD->mode = MODE_FIXED;
		}

		else {
			free(pBD->cb_head);
			pBD->cb_head = NULL;
			free(pBD);
			pBD = NULL;
			return NULL;
		}
		pBD->capacity = DEFAULT_INIT_CAPACITY;
	}

	if (init_capacity != 0) {

		pBD->cb_head = (char*)malloc(sizeof(char) * init_capacity);
		if (pBD->cb_head == NULL) {
			free(pBD);
			return NULL;
		}

		if (o_mode == 'f' || inc_factor == INC_FACTOR_F) {
			pBD->mode = MODE_FIXED;
			pBD->inc_factor = INC_FACTOR_F;
		}
		else if (o_mode == 'a' && inc_factor <= INC_FACTOR_ADD_MAX && (unsigned char)inc_factor >= INC_FACTOR_ADD_MIN) {
			pBD->mode = MODE_ADD;
			pBD->inc_factor = inc_factor;
		}
		else if (o_mode == 'm' && inc_factor <= INC_FACTOR_MULTI_MAX && (unsigned char)inc_factor >= INC_FACTOR_MULTI_MIN) {
			pBD->mode = MODE_MULTI;
			pBD->inc_factor = inc_factor;
		}
		else {
			free(pBD->cb_head);
			pBD->cb_head = NULL;
			free(pBD);
			pBD = NULL;
			return NULL;
		}
		pBD->capacity = init_capacity;
	}

	pBD->flags = DEFAULT_FLAGS;

	return pBD;
}

/*****************************************************************************************************************
Purpose: Set the r_flag bit to 0/1 and add a new character to the Buffer. Resize if needed.
Author: Vidhi Patel
History/Versions: 1.0
Called functions: realloc()
Parameters: pBuffer const pBD, char symbol
Return value: pBuffer pBD
Algorithm: - Set the r_flag bit to 0 using bit wise.
		   - If the buffer is not full, add the symbol and increment addc_offset by one
		   - If the buffer is full, resize the buffer based on the operation mode.
		   - Return pointer to the Buffer if succeeded, else return NULL and free dynamically allocated memory
********************************************************************************************************************/

pBuffer b_addc(pBuffer const pBD, char symbol) {

	short newCapacity; /*New memory size for the array reallocation*/
	short availableSpace; /*Available space inside the array*/
	short newIncrement; /*Value of the expanded size of the array*/
	char* newBuffer; /*point to the head of the new buffer array*/
	char* oldBuffer; /*point to the head of the old buffer array*/
	newCapacity = 0; /*initialize*/

	/*Check if pointer to the Buffer is not NULL*/
	if (pBD == NULL) {
		return NULL;
	}

	/*Reset the Flag r_flag to 0*/
	pBD->flags &= RESET_R_FLAG;

	/*If buffer is full, increase the the current capacity based on the operation mode*/
	if ((short)(pBD->addc_offset * sizeof(char)) >= pBD->capacity) {

		/*Fixed-size Mode*/
		if (pBD->mode == MODE_FIXED) {
			return NULL;
		}

		/*Additive self-incrementing mode*/
		if (pBD->mode == MODE_ADD) {
			/*Adding inc_factor (converted in bytes) to capacity for increment*/
			newCapacity = pBD->capacity + (char)pBD->inc_factor;

			if (newCapacity > 0 && newCapacity > MAX_CAP) {
				newCapacity = MAX_CAP;
			}
			else if (newCapacity < 0) {
				return NULL;
			}
		}

		/*Multiplicative self-incrementing mode*/
		if (pBD->mode == MODE_MULTI) {
			/*Reached maximum capacity*/
			if (pBD->capacity >= MAX_CAP) {
				return NULL;
			}

			/*Calculate the new capacity*/
			availableSpace = (MAX_CAP)-pBD->capacity;
			newIncrement = (short)(availableSpace * (pBD->inc_factor / 100));
			newCapacity = pBD->capacity + newIncrement;

			/*Check if increment is okay*/
			if (newIncrement <= 0 && pBD->capacity < MAX_CAP) {
				newCapacity = MAX_CAP;
			}
			else if (newCapacity > MAX_CAP || newCapacity < 0) {
				return NULL;
			}
		}

		/*When capacity increment in 1 or -1 is successful*/
		oldBuffer = pBD->cb_head;
		newBuffer = (char*)realloc(pBD->cb_head, newCapacity);

		/*Check if the new buffer allocation is done properly or not*/
		if (newBuffer == NULL) {
			return NULL;
		}

		/*Check if the memory is reallocated or not*/
		if (newBuffer != oldBuffer) {
			pBD->flags |= SET_R_FLAG;
		}

		/*Update the new capacity and pointer to the head of the array*/
		pBD->capacity = newCapacity;
		pBD->cb_head = newBuffer;

	}
	/*Add the character to the Buffer Character Array*/
	pBD->cb_head[pBD->addc_offset] = symbol;
	pBD->addc_offset++;
	return pBD;
}

/************************************************************************************************************
Purpose:					Clear the Buffer and retain all memory space currently allocatd to the buffer
Author:						Vidhi Patel
History/Versions:			1.0
Called functions:			None
Parameters:					Buffer* const pBD
Return value:				1 or -1
Algorithm:					- Check if the pointer to the Buffer is working, else return -1
							- Reset the Buffer addc_offset, flags, getc_offset, markc_offset. Return 1 to notify success.
************************************************************************************************************/
int b_clear(Buffer* const pBD) {
	/*Check the pointers to the array*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*Reset the value of offsets and flags*/
	pBD->flags = pBD->flags & DEFAULT_ZERO;
	pBD->flags = pBD->flags | DEFAULT_FLAGS;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->addc_offset = 0;

	return 1;
}

/************************************************************************************************************
Purpose:					Free all memory inside the Buffer
Author:						Vidhi Patel
History/Versions:			1.0
Called functions:			free()
Parameters:					Buffer* const pBD
Return value:				None
Algorithm:					- Check if the pointer is NULL. If not, free the dynamically allocate memory for the
							character array and the Buffer structure.
************************************************************************************************************/
void b_free(Buffer* const pBD) {
	/*Check the pointers and free the character array. Set cb_head to NULL to prevent dangling pointer*/
	if (pBD->cb_head != NULL) {
		free(pBD->cb_head);
		pBD->cb_head = NULL;
	}

	/*Free the Buffer structure*/
	free(pBD);
}

/************************************************************************************************************
Purpose:				Check the capacity of the character buffer
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				Buffer *const pBD
Return value:			1, 0, -1
Algorithm:				- Check if the pointer to the Buffer is working, if not, return -1
						- Check the value of the Buffer capacity. If it is full return 1, if not return 0
************************************************************************************************************/
int b_isfull(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*Check the character buffer size*/
	if ((short)(pBD->addc_offset * sizeof(char)) == pBD->capacity) {
		return 1;
	}

	return 0;
}

/**********************************************************************************************************************************
Purpose:				Check the limit of the character buffer
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				Buffer *const pBD
Return value:			short addc_offset, 1
Algorithm:				- Check if the pointer to the Buffer is working, if not, return -1
						- Check the value of the Buffer addc_offset, return the value.
***********************************************************************************************************************************/
short b_addcoffset(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return pBD->addc_offset;
}


/**********************************************************************************************************************************
Purpose:				Return the capacity of the character buffer
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				Buffer *const pBD
Return value:			short capacity
Algorithm:				- Check if the pointer to the Buffer is working, if not, return -1
						- Check the value of the Buffer capacity. Return the value
**********************************************************************************************************************************/

short b_capacity(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	return pBD->capacity;
}


/********************************************************************************************************************************
Purpose:				Set markc_offset to mark
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				pBuffer const pBD
Return value:			short markc_offset, NUll
Algorithm:				- Check if the pointer to the Buffer is working, if not, return null
						- Check the value of the mark param. If it is within the limt of the buffer, return markc_offset, else return null
********************************************************************************************************************************/
short b_markc(pBuffer const pBD, short mark)
{
	if (NULL == pBD || !(mark >= 0 && mark <= b_addcoffset(pBD)))
	{
		return RT_FAIL_1;
	}
	else
	{
		pBD->markc_offset = mark;
		return mark;
	}
}

/********************************************************************************************************************************
Purpose:				Return the value of mode
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				Buffer *const pBD
Return value:			1, int mode
Algorithm:				- Check if the pointer to the Buffer is working, if not, return -1
						- Check the value of the Buffer capacity. If it is full return 1, if not return 0
*************************************************************************************************************************/
int b_mode(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return (int)pBD->mode;
}

/*************************************************************************************************************************
Purpose:				Return the non-negative value of inc_factor
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				Buffer *const pBD
Return value:			inc_factor or 0x100 (256 in size_t)
Algorithm:				- Check if the pointer to the Buffer is working, if not, return 0x100
						- Check the value of the Buffer inc_factor. Return the value inc_factor (casted to size_t)
**************************************************************************************************************************/
size_t b_incfactor(Buffer* const pBD) {
	/*Check the pointer of the Buffer. If failed, return 0x100 (256 in size_t)*/
	if (pBD == NULL) {
		return (size_t)INC_FACTOR_FAIL;
	}

	return (size_t)(unsigned char)(pBD->inc_factor);
}

/************************************************************************************************************************
Purpose:			Load an open input file into the Buffer
Author:				Vidhi Patel
History/Versions:	1.0
Called functions:	fgetc(), feof(), ungetc(), b_addc()
Parameters:			Buffer *const pBD, FILE* const fi
Return value:		-1, -2 or numbers of character added to the Buffer (num)
Algorithm:			- Check the Buffer pointer and the file pointer. Return -1 if failed
					- Read the file and add the character to the Buffer. Return -2 if unable to added
					  , else return the number of characters added to the Buffer
*************************************************************************************************************************/
int b_load(FILE* const fi, Buffer* const pBD) {
	char c; /*hold a character from a file*/
	int num; /*number of characters inside the file*/
	num = 0; /*initialize*/

	/*Check the Buffer pointer and the file pointer*/
	if (pBD == NULL || fi == NULL) {
		return RT_FAIL_1;
	}

	/*A while loop to get the character inside the file, break the loop when encounters EOF*/
	while (1) {
		/*Get the character from the file*/
		c = (char)fgetc(fi);

		/*Check for EOF using feof()*/
		if (feof(fi)) {
			break;
		}

		/*If the character cannot be added, return -2*/
		if (b_addc(pBD, c) == NULL) {
			ungetc(c, fi);
			return LOAD_FAIL;
		}
		num++;
	}

	return num;
}

/**********************************************************************************************************************
Purpose:					Return the empty state of the Buffer
Author:						Vidhi Patel
History/Versions:			1.0
Called functions:			None
Parameters:					Buffer *const pBD
Return value:				1, 0, -1
Algorithm:					- Check if the pointer to the Buffer is working, if not, return -1
							- Check the value of the addc_offset. If it is 0, return 1, other wise return 0.
***********************************************************************************************************************/
int b_isempty(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*Check the value of the addc_offset*/
	if (pBD->addc_offset == 0) {
		return 1;
	}

	return 0;
}

/**********************************************************************************************************************
Purpose:				Read the Buffer
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		None
Parameters:				Buffer *const pBD
Return value:			-2, 0
Algorithm:				- Check if the pointer to the Buffer is working, if not, return -2
						- Check the value of getc_offset and addc_offset, set flags field eob to 1, return 0. Otherwise,
						  set eob to 0
						- Return character located at getc_offset, increment getc_offset by 1
***********************************************************************************************************************/
char b_getc(Buffer* const pBD) {
	/*Check the pointer of the Buffer. If failed, reuturn -2*/
	if (pBD == NULL) {
		return RT_FAIL_2;
	}

	/*Check if we reached the end of the buffer. Set the flags EOB bit to 1 if reached the end, else 0*/
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags |= SET_EOB;
		return 0;
	}
	else {
		pBD->flags &= RESET_EOB;
	}
	/*Increment getc_offset by one. Return character at position getc_offset inside the character array*/
	return pBD->cb_head[pBD->getc_offset++];
}

/*************************************************************************************************************
Purpose:						Return the value of flags field eob bit.
Author:							Vidhi Patel
History/Versions:				1.0
Called functions:				None
Parameters:						Buffer *const pBD
Return value:					Value of flags determined by the eob bit or -1
Algorithm:						- Check if the pointer to the Buffer is working, if not, return -1
								- Check the value of flag determined by the eob bit, using bitwise operation
**************************************************************************************************************/
int b_eob(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	return (int)pBD->flags & CHECK_EOB;
}

/**********************************************************************************************************
Purpose:				Print character of the Buffer one by one. For diagnostic purpose only
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		b_eob(), b_getc(), b_isempty()
Parameters:				Buffer *const pBD, char nl
Return value:			Return the number of characters printed, else return -1 on failure
Algorithm:				- Check if the pointer to the Buffer is working, if failed, return -1
						- In a while loop, use b_getc() to get the character from the character array. Use
						  b_eob() to detect the end of the buffer and break the loop. Print the character to
						  the standard output.
						- Return the numbers of characters printed. If specified, print out a new line character
***********************************************************************************************************/

int b_print(Buffer* const pBD, char nl) {
	int num; /*Number of characters printed*/
	char c; /*The character to be printed out*/
	num = 0; /*Initialize*/

	/*Check the pointers of the buffer and the character array*/
	if (pBD == NULL || pBD->cb_head == NULL) {
		return RT_FAIL_1;
	}

	/*A while loop to get the character of the array and print it to the standard output*/
	while (1) {
		/*Get the character from the character array*/
		c = b_getc(pBD);

		/*If we reached the end of the buffer, break the loop*/
		if (b_eob(pBD)) {
			break;
		}

		/*Print the character to the standard output*/
		printf("%c", c);
		num++;
	}

	/*If specified, prints a new line character*/
	if (nl != 0)
		printf("\n");

	return num;
}

/*********************************************************************************************************************
Purpose:			Use realloc() to adjust the new capacity and update all necessary features of the Buffer descriptor
Author:				Vidhi Patel
History/Versions:	1.0
Called functions:	realloc(), sizeof()
Parameters:			Buffer *const pBD, char symbol
Return value:		Return the new pointer to the Buffer, else return NULL
Algorithm:			- Check if the pointer to the Buffer or the cb_head is working, if failed, return NULL
					- Using realloc() to increase the size of cb_head array, and add the symbol to the end of the
					  Buffer character array, increment addc_offset by one.
***********************************************************************************************************************/

Buffer* b_compact(Buffer* const pBD, char symbol) {
	short newCapacity; /*New capacity for the buffer array reallocation*/
	char* newBuffer; /*Pointer to the head of the new array*/
	newCapacity = 0; /*Initialize*/

	/*Check the pointers of the Buffer and the character array*/
	if (pBD == NULL || pBD->cb_head == NULL) {
		return NULL;
	}

	/*Calculate the new capacity*/
	newCapacity = (short)(sizeof(char) * (pBD->addc_offset + 1));

	/*Check newCapacity*/
	if (newCapacity <= 0) {
		return NULL;
	}

	/*Readjust the characer array size*/
	newBuffer = (char*)realloc(pBD->cb_head, newCapacity);

	/*Check if the new buffer is allocated properly*/
	if (newBuffer == NULL) {
		return NULL;
	}

	/*Check if the Buffer is reallocated or not*/
	if (newBuffer != pBD->cb_head) {
		pBD->flags |= SET_R_FLAG;
	}

	/*Update the pointer to the character array and the capacity. */
	pBD->cb_head = newBuffer;
	pBD->cb_head[pBD->addc_offset] = symbol;
	pBD->addc_offset++;
	pBD->capacity = newCapacity;
	return pBD;
}

/***********************************************************************************************
Purpose:					returns the value of flags determined by the r_flag bit
Author:						Vidhi Patel
History/Versions:			1.0
Called functions:			none
Parameters:					Buffer *const pBD
Return value:				Return the value of the flags field, if error, return -1
Algorithm:					- Check if the pointer to the Buffer, if failed, return -1
							- Using bitwise operation to return the value of the flags field.
***********************************************************************************************/
char b_rflag(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	return (char)(pBD->flags & CHECK_R_FLAG);
}

/************************************************************************************************************
Purpose:				Decrements the value of getc_offset by one
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		none
Parameters:				Buffer *const pBD
Return value:			getc_offset, -1
Algorithm:				- Check the pointer to the Buffer and getc_offset, if failed, return -1
						- Decrement getc_offset by one, return getc_offset
************************************************************************************************************/
short b_retract(Buffer* const pBD) {
	/*Check the pointer of the buffer and value of getc_offset*/
	if (pBD == NULL || pBD->getc_offset <= 0) {
		return RT_FAIL_1;
	}

	pBD->getc_offset--;
	return pBD->getc_offset;
}

/************************************************************************************************************
Purpose:				Set the getc_offset to the value of the current markc_offset
Author:					Vidhi Patel
History/Versions:		1.0
Called functions:		none
Parameters:				Buffer *const pBD
Return value:			getc_offset, -1
Algorithm:				- Check if the pointer to the Buffer, if failed, return -1
						- Set the value of getc_offset to markc_offset, returns getc_offset if no problem occurs.
						- If markc_offset smaller than 0 or larger than addc_offset, return -1
************************************************************************************************************/
short b_reset(Buffer* const pBD) {
	/*Check the pointer of the Buffer and value of markc_offset*/
	if (pBD == NULL || pBD->markc_offset < 0 || pBD->markc_offset > pBD->addc_offset) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/************************************************************************************************************
Purpose:			Return getc_offset value to the calling function
Author:				Vidhi Patel
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer *const pBD
Return value:		getc_offset, -1
Algorithm:			- Check if the pointer to the Buffer, if failed, return -1
					- Returns getc_offset if no problem occurs.
					- If getc_offset smaller than 0 or larger than addc_offset, return -1
************************************************************************************************************/
short b_getcoffset(Buffer* const pBD) {
	/*Check the pointer of the Buffer and valud of the getc_offset*/
	if (pBD == NULL || pBD->getc_offset < 0 || pBD->getc_offset > pBD->addc_offset) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/************************************************************************************************************
Purpose:					Set the getc_offset and markc_offset to 0 so that it can reread again.
Author:						Vidhi Patel
History/Versions:			1.0
Called functions:			none
Parameters:					Buffer *const pBD
Return value:				0, -1
Algorithm:					- Check if the pointer to the Buffer, if failed, return -1
							- Set the value of getc_offset and markc_offset to 0, returns 0 if no problem occurs.
************************************************************************************************************/

int b_rewind(Buffer* const pBD) {
	/*Check the pointer of the Buffer*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}

/************************************************************************************************************************
Purpose:			returns pointer to the location of a particular character
Author:				Vidhi Patel
History/Versions:	1.0
Called Function:	-
Parameters:			pBD - buffer structure, loc_offset - location information of a particular character
Return Value:		if pBD is equal to NULL it returns NULL otherwise returns pointer to the particular character
Algorithm:			adds loc_offset to cb_head and returns the value.
************************************************************************************************************************/

char* b_location(Buffer* const pBD, short loc_offset) {
	/*checking if Buffer is operational to prevent unexpected crashes*/
	if (pBD == NULL) {
		return NULL;
	}
	/*returning cb_head + the value of loc_offset*/
	return (pBD->cb_head + loc_offset);
}