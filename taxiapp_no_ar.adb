-- program without autorefresh
-- linux version

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
use Ada.Text_IO;
use Ada.Strings.Fixed;

procedure taxiapp_no_ar is

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

	task type Worker is
		entry Get_Id (New_Id, Rest : in Natural);
		entry Get_Order (New_Order : in Natural);
		entry Accept_Order;
	end Worker;
	
	task body Worker is
		Id : Natural;
		Order : Natural;
		Traveled : Natural;
		RestLevel : Natural;
		Completion : Float;
		JanushFlag : Boolean;
		
	begin
		accept Get_Id (New_Id, Rest : in Natural) do
			Id := New_Id;
			RestLevel := Rest;
		end Get_Id;
		
		if RandGen.generate_random_number(10) = 5 then
			JanushFlag := TRUE;
		else
			JanushFlag := FALSE;
		end if;
		
		loop
			accept Get_Order (New_Order : in Natural) do
				Order := New_Order;
			end Get_Order;
			
			exit when Order = 0;
			
			if JanushFlag and Order < 10 then
				delay 1.0;
			else			
				accept Accept_Order;
				
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
			end if;
		end loop;
	end Worker;
----------------------- DRIVERS END ---------------------	

begin

	Mainloop:
	declare
		RestValue : Positive;
		Workers : array(WorkersCount) of Worker;
		Value : Integer;
		DriverStatus : Integer;
		SelectedDriver : Natural;
		DriverRoute : array(WorkersCount) of Integer;
		DriverRests : Boolean;
	begin
		Put_Line ("#MANAGER Wybierz poziom ochoty do pracy kierowcow");
		Put_Line ("(1-10, gdzie 1 to najmniejsze przerwy)");
		Value := Parse_String (ReadStr);
		
		if Value < 1 or Value > 10 then
			RestValue := RandGen.generate_random_number(10) + 1;
		else
			RestValue := Value;
		end if;
		Put_Line ("#MANAGER Wybrano poziom" & RestValue'Img);
		
		for I in WorkersCount loop
			AreBusy(I) := FALSE;
			Workers(I).Get_Id(I, RestValue);
			WorkerStatus(I) := 0;
			DriverRoute(I) := 0;
		end loop;
		
		CountActive := 0;
		SelectedDriver := 1;
		Put_Line("#MANAGER Kierowcy przygotowani");
		Put_Line("#MANAGER Wcisnij enter aby rozpoczac");
		Value := Parse_String (ReadStr);
		
		loop
			Put (ASCII.ESC & "c");
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
				elsif DriverRoute(I) = -10 then
					Put_Line (" Kurla za mniej niz 10 mnie sie nie oplaca");
					DriverRoute(I) := 0;
				elsif not AreBusy(I) then
					Put_Line (" Kierowca wolny");
				else
					Put_Line (" " & TrimInt(DriverStatus*5) & "% z" & DriverRoute(I)'Img & "zj");
				end if;
				
			end loop;
---------------- print drivers states end -----------------
			
			Put_Line ("Wybierz dlugosc trasy w zj (zlotowajednostki) (-1 aby zakonczyc):");

			Value := Parse_String (ReadStr);
			exit when Value = -1;
			
			if Value > 0 then

				if CountActive < WorkersCount'Last then
					while AreBusy(SelectedDriver) loop
						
						SelectedDriver := SelectedDriver + 1;
						if SelectedDriver > 10 then
							SelectedDriver := 1;
						end if;
					end loop;
					
					DriverRests := FALSE;
					select
						Workers(SelectedDriver).Get_Order(Value);
						select 
							Workers(SelectedDriver).Accept_Order;
							DriverRoute(SelectedDriver) := Value;
							AreBusy(SelectedDriver) := TRUE;
							CountActive := CountActive + 1;
						or
							delay 0.05;
							DriverRoute(SelectedDriver) := -10;
						end select;
					or
						delay 0.2;
						DriverRests := TRUE;
					end select;
					
					if DriverRests then
						Put_Line ("Kierowca nr" & SelectedDriver'Img & " jeszcze odpoczywa.");
						delay 1.0;
					end if;
					
					SelectedDriver := SelectedDriver + 1;
					if SelectedDriver > 10 then
						SelectedDriver := 1;
					end if;
				
				else 
					New_Line;
					Put_Line ("All drivers are busy now, please try again later");
					delay 2.0;
				end if;
			end if;
			
		end loop;
		Put_Line ("#MANAGER Zwalniam kierowcow");
		for I in WorkersCount loop
			select
				Workers(I).Get_Order(0);
			or
				delay 0.05;
				abort Workers(I);
			end select;
		
		end loop;
		
		Put_Line ("#MANAGER koncze prace");
	end Mainloop;
	
end;
