:- protocol(class_elements_generator_protocol).
   :- public(prepare_init_assignments/2).
   :- private(prepare_init_assignment/2).
   :- private(prepare_class_code/3).
   :- private(prepare_class_function_code/3).
:- end_protocol.

:- category(class_elements_generator,
   implements([class_elements_generator_protocol])).
   

   prepare_init_assignments([], '') :- !.
   prepare_init_assignments([Attr|Rest], Assignments) :-
            prepare_init_assignment(Attr, Assignment),
            prepare_init_assignments(Rest, RestAssignments),
            atomic_list_concat([Assignment, RestAssignments], '\n', Assignments).
   
   prepare_init_assignment('', '').
   prepare_init_assignment(Attr, Assignment) :-
      atomic_list_concat(['\t\tself.', Attr, ' = ', Attr], '', Assignment).
   
   prepare_class_code(ClassName, Attributes, Code) :-
      (Attributes = '' ->
         CodeLines = [
            "class ", ClassName, ":\n",
            "\tdef __init__(self):\n",
            "\t\tpass\n"
         ]
      ;  
         (Attributes = [] -> 
            CodeLines = [
               "class ", ClassName, ":\n",
               "\tdef __init__(self):\n",
               "\t\tpass\n"
            ]
         ;
            prepare_init_assignments(Attributes, Assignments),
            atomic_list_concat(Attributes, ', ', AttributesStr),
            CodeLines = [
               "class ", ClassName, ":\n",
               "\tdef __init__(self, ", AttributesStr, "):\n",
               Assignments, "\n"
            ]
         )
      ),
     atomic_list_concat(CodeLines, Code).

   prepare_class_function_code(FunctionName, Attributes, Code) :-
      (Attributes = '' ->
         CodeLines = [
             "def ", FunctionName, "(self):\n",
             "\tpass\n"
         ]
     ;   
         (Attributes = [] -> 
            CodeLines = [
               "def ", FunctionName, "(self):\n",
               "\tpass\n"
            ]
         ;
            atomic_list_concat(Attributes, ', ', AttributesStr),
            CodeLines = [
               "def ", FunctionName, "(self, ", AttributesStr, "):\n",
               "\tpass\n"
            ]
         )
     ),
      atomic_list_concat(CodeLines, Code).
:- end_category.


:- protocol(input_protocol).
   :- private(get_class_name/1).
   :- private(get_function_name/1).
   :- private(get_attributes/1). 
:- end_protocol.

:- category(keyboard_input,
   implements([input_protocol])).

   get_class_name(ClassName):-
      write('Enter the class name in double or single quotes: '),
      read(ClassName).

   get_function_name(FunctionName):-
      write('Enter the function name in double or single quotes: '),
      read(FunctionName).   

   get_attributes(Attributes):-
      write('Enter the attribute names separated by commas or enter '': '),
      read(AttributesTerm),
      term_string(AttributesTerm, AttributesStr),
      split_string(AttributesStr, ',', "\n\t\s", Attributes).

:- end_category.


:- protocol(generators_protocol).
   :- public(generate_class_function_code/0).
   :- public(generate_class_function_code/2). 
   :- public(generate_class_code/0).
   :- public(generate_class_code/2). 
:- end_protocol.

:- object(python_code_generator,
   implements([generators_protocol]),
   imports([keyboard_input, class_elements_generator])).

   :- info([
      version is 0:0:1,
      author is 'Andrey Levkov',
      date is 2023-12-11,
      comment is 'Best python code generator'
   ]).
   
   generate_class_function_code :-
      ::get_function_name(FunctionName),
      ::get_attributes(Attributes),
      generate_class_function_code(FunctionName, Attributes).

   generate_class_function_code(FunctionName, Attributes) :-
      ::prepare_class_function_code(FunctionName, Attributes, FunctionCode),
      write(FunctionCode), nl.
   
   generate_class_code:-
      ::get_class_name(ClassName),
      ::get_attributes(Attributes),
      ::prepare_class_code(ClassName, Attributes, ClassCode),
      write(ClassCode), nl.

   generate_class_code(ClassName, Attributes):-
      ::prepare_class_code(ClassName, Attributes, ClassCode),
      write(ClassCode), nl.
   
:- end_object.
