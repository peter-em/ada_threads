with Ada.Text_IO;
with Ada.Strings.Fixed;
with NT_Console;
with Ada.Numerics.Discrete_Random;
use Ada.Text_IO;
use Ada.Strings.Fixed;

procedure taxiapp is

	package NT renames NT_Console;
	subtype WorkersCount is Integer range 1 .. 10;
	package RandGen is
		function generate_random_number (n : in Natural) return Natural;
	end RandGen;
	
	package body RandGen is
		subtype Rand_Range is Natural;
		package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
	
		gen : Rand_Int.Generator;
		
		function generate_random_number (n : in Natural) return Natural is
		begin
			return Rand_Int.Random(gen) mod n;
		end generate_random_number;
		
	begin
		Rand_Int.Reset(gen);
	end RandGen;
	
	function TrimInt (Val : in Integer) return String is
	(Ada.Strings.Fixed.Trim (Integer'Image(Val), Ada.Strings.Left));
	
	function Parse_String (Str : in String) return Integer is
	begin
		return Integer'Value (Str);
		exception
			when Constraint_Error => return 0;
	end Parse_String;
	
	function ReadStr return String is
		Str : String (1 .. 4);
		Last : Natural;
	begin
		Get_Line (Str, Last);
		if Last = Str'Last then
			Skip_Line;
		end if;
		return Str (1 .. Last);
	end ReadStr;

	
	AreBusy : array(WorkersCount) of Boolean;
	pragma Atomic_Components(AreBusy);
	CountActive : Integer;
	pragma Atomic(CountActive);
	WorkerStatus : array(WorkersCount) of Integer;
	pragma Atomic_Components(WorkerStatus);
	
----------------------- DRIVERS START -------------------------------	
	task type Procces_Data is
		entry Input (Val : in Integer);
		entry Start (Flag : in Boolean);
	end Procces_Data;

	task type Worker is
		entry Get_Id (New_Id, Rest : in Natural);
		entry Get_Order (New_Order : in Natural);
	end Worker;
	
	
	
	task body Worker is
		Id : Natural;
		Order : Natural;
		Traveled : Natural;
		RestLevel : Natural;
		Completion : Float;
		
	begin
		accept Get_Id (New_Id, Rest : in Natural) do
			Id := New_Id;
			RestLevel := Rest;
		end Get_Id;
		loop
			--Put_Line ("Kierowca Nr " & Id'Img & " czekam na zlecenie");
			accept Get_Order (New_Order : in Natural) do
				
				Order := New_Order;
			end Get_Order;
			
			exit when Order = 0;
			--Put_Line ("Kierowca Nr " & Id'Img & " wyrusza w trase");
			Traveled := 0;
			while Traveled < Order loop
				delay 1.0;
				Traveled := Traveled + 1;
				Completion := Float (Traveled * 20) / Float (Order);
				WorkerStatus(Id) := Integer(Completion);
				
			end loop;
			AreBusy(Id) := FALSE;
			CountActive := CountActive - 1;
			delay 1.0 * (RandGen.generate_random_number(RestLevel) + 1);
			
			--Put_Line ("Kierowca Nr " & Id'Img & " dojechal do celu");
			
		end loop;
		--Put_Line ("Jestem kierowca Nr " & Id'Img & ", koncze prace");
	end Worker;

----------------------- DRIVERS END ---------------------	

----------------------- MANAGER START -------------------
	
		
	task body Procces_Data is
		Workers : array(WorkersCount) of Worker;
		Value : Integer;
		DriverStatus : Integer;
		SelectedDriver : Natural;
		RestLevel : Natural;
		DriverRoute : array(WorkersCount) of Integer;
		GoFlag : Boolean := FALSE;
	begin
		
		accept Start (Flag : in Boolean) do
			GoFlag := Flag;
			RestLevel := RandGen.generate_random_number(10) + 1;
			Put_Line ("Wybrano poziom" & RestLevel'Img);
		end Start;
		
		if GoFlag then
		for I in WorkersCount loop
			AreBusy(I) := FALSE;
			Workers(I).Get_Id(I, RestLevel);
			WorkerStatus(I) := 0;
			DriverRoute(I) := 0;
		end loop;
		CountActive := 0;
		SelectedDriver := 1;
		Put_Line("#MANAGER Kierowcy przygotowani");
		Put_Line("#MANAGER Przerwa 2s");
		delay 2.0;
		
		loop
			NT.Clear_Screen;
---------------- print drivers states begin ---------------
			Put_Line ("Aktywni kierowcy:" & CountActive'Img & ". Ich status:");
			for I in WorkersCount loop
				DriverStatus := WorkerStatus(I);
				
				Put ("Nr");
				Put (I'Img);
				Set_Col(7);
				Put ("[");				
				Put (DriverStatus*'%');
				Put ((20-DriverStatus)*'-');
				Put (']');	
				
				if DriverStatus = 20 then
					Put_Line (" Kierowca dojechal do celu");
					WorkerStatus(I) := 0;
				elsif not AreBusy(I) then
					Put_Line (" Kierowca wolny");					
				else
					Put_Line (" " & TrimInt(DriverStatus*5) & "% z" & DriverRoute(I)'Img & "zj");
				end if;
				
			end loop;
---------------- print drivers states end -----------------
			
			Put_Line ("Wybierz dlugosc trasy w zj (zlotowajednostki) (-1 aby zakonczyc):");

			select
				accept Input (Val : in Integer) do
					Value := Val;
				end Input;

			or
				delay 3.5;
				New_Line;
				Value := 0;
			end select;
			exit when Value = -1;
			
			if Value > 0 then

				if CountActive < WorkersCount'Last then
					while AreBusy(SelectedDriver) loop
						
						SelectedDriver := SelectedDriver + 1;
						if SelectedDriver > 10 then
							SelectedDriver := 1;
						end if;
						--Put_Line ("Szukam");
							
					end loop;
					
					
					
					select
						Workers(SelectedDriver).Get_Order(Value);
						DriverRoute(SelectedDriver) := Value;
						AreBusy(SelectedDriver) := TRUE;
						CountActive := CountActive + 1;
					or
						delay 0.2;
						Put_Line ("Kierowca nr" & SelectedDriver'Img & " jeszcze odpoczywa.");
					end select;
				
				
				else 
					New_Line;
					Put_Line ("All drivers are busy now, please try again later");
					delay 0.8;
				end if;
			end if;
			delay 1.0;
			
		end loop;
		Put_Line ("#MANAGER zwalniam kierowcow");
		for I in WorkersCount loop
			--Workers(I).Get_Order(0);
			abort Workers(I);
		end loop;
		end if;
		Put_Line ("#MANAGER koncze prace");
	end Procces_Data;
----------------------- MANAGER END ----------------------


begin
	--Put_Line ("Losowa liczba:" & RandGen.generate_random_number(10)'Img);

	Mainloop:
	declare
		MValue : Integer;
		Manager : Procces_Data;
		--RestValue : Positive;
	begin
		--Put_Line ("Wybierz poziom ochoty do pracy kierowcow (1-10, gdzie 1 to najmniejsze przerwy)");
		--MValue := Parse_String (ReadStr);
		--if MValue < 1 or MValue > 10 then
		--	RestValue := RandGen.generate_random_number(10) + 1;
		--else 
		--	RestValue := MValue;
		--end if;
		--Put_Line ("Wybrano poziom" & RestValue'Img);
		Manager.Start(TRUE);
		loop		
			MValue := Parse_String (ReadStr);
			Manager.Input(MValue);		
			exit when MValue = -1;

		end loop;
	end Mainloop;
end;
