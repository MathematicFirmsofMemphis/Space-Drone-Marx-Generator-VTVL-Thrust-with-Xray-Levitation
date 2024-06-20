using System;

namespace SpaceDrone
{
	class Program
	{
		static void Main(string[] args)
		{
			// Initialize the drone
			MarxGeneratorDrone drone = new MarxGeneratorDrone(10f, 5f);

			// Set an initial direction (e.g., Vertical Takeoff Vertical Landing)
			Vector3 initialDirection = new Vector3(0, 1, 0);
			drone.ChangeDirection(initialDirection);

			// Simulate drone movement for a few vectors
			for (int i = 0; i < 10; i++)
			{
				drone.ApplyLevitation();
				Console.WriteLine($"Step {i+1}: Position = {drone.Position}");
			}
		}
	}

	public class MarxGeneratorDrone
	{
		public float LevitationThrust { get; private set; }
		public float ArcLength { get; private set; }
		public Vector3 Direction { get; private set; }
		public Vector3 Position { get; private set; }

		public MarxGeneratorDrone(float levitationThrust, float arcLength)
		{
			LevitationThrust = levitationThrust;
			ArcLength = arcLength;
			Direction = new Vector3(0, 1, 0); // Default direction is upward
			Position = new Vector3(0, 0, 0); // Initial position
		}

		public void ApplyLevitation()
		{
			// Simulate levitation by updating the position based on the levitation Thrust and direction
			Vector3 levitationVector = Direction.Normalized() * LevitationThrust;
			Position += levitationVector;

			// Debugging output to visualize the levitation vector
			Console.WriteLine($"Applying levitation: Thrust = {levitationVector}");
		}

		public void ChangeDirection(Vector3 newDirection)
		{
			Direction = newDirection;
		}
	}

	public struct Vector3
	{
		public float X { get; private set; }
		public float Y { get; private set; }
		public float Z { get; private set; }

		public Vector3(float x, float y, float z)
		{
			X = x;
			Y = y;
			Z = z;
		}

		public static Vector3 operator +(Vector3 a, Vector3 b)
		{
			return new Vector3(a.X + b.X, a.Y + b.Y, a.Z + b.Z);
		}

		public static Vector3 operator *(Vector3 a, float scalar)
		{
			return new Vector3(a.X * scalar, a.Y * scalar, a.Z * scalar);
		}

		public Vector3 Normalized()
		{
			float length = (float)Math.Sqrt(X * X, Y * Y, Z * Z);
			return new Vector3(X / length, Y / length, Z / length);
		}

		public override string ToString()
		{
			return $"({X}, {Y}, {Z})";
		}
	}
}


//Implementation in Fortran

program space_drone

implicit none

type :: Vector3
real :: x
real :: y
real :: z
contains
procedure :: norm => normalize
procedure :: print => print_vector
end type Vector3

type :: MarxGeneratorDrone
real :: levitationThrust
real :: arcLength
type(Vector3) :: direction
type(Vector3) :: position
contains
procedure :: applyLevitation
procedure :: changeDirection
end type MarxGeneratorDrone

type(MarxGeneratorDrone) :: drone
type(Vector3) :: initialDirection
integer :: i

! Initialize the drone
drone%levitationThrust = 10.0
	drone%arcLength = 5.0
	initialDirection = Vector3(0.0, 1.0, 0.0)
	call drone%changeDirection(initialDirection)

	! Simulate drone movement for a few vectors
		do i = 1, 10
	call drone%applyLevitation()
	print *, "Step ", i, ": Position = "
	call drone%position%print()
	end do

		contains

		subroutine applyLevitation(this)
		class(MarxGeneratorDrone), intent(inout) :: this
		type(Vector3) :: levitationVector

		! Simulate levitation by updating the position based on the levitation Thrust and direction
		levitationVector = this%direction%norm() * this%levitationThrust
			this%position = this%position + levitationVector

			! Debugging output to visualize the levitation vector
			print *, "Applying levitation: Thrust = "
	call levitationVector%print()
	end subroutine applyLevitation

	subroutine changeDirection(this, newDirection)
	class(MarxGeneratorDrone), intent(inout) :: this
	type(Vector3), intent(in) :: newDirection

	this%direction = newDirection
	end subroutine changeDirection

	function normalize(v) result(res)
	type(Vector3), intent(in) :: v
type(Vector3) :: res
real :: length

length = sqrt(v%x**2 + v%y**2 + v%z**2)
	res%x = v%x / length
	res%y = v%y / length
	res%z = v%z / length
	end function normalize

	function vector_add(v1, v2) result(res)
	type(Vector3), intent(in) :: v1, v2
type(Vector3) :: res

res%x = v1%x + v2%x
	res%y = v1%y + v2%y
	res%z = v1%z + v2%z
	end function vector_add

	function vector_scalar_multiply(v, scalar) result(res)
	type(Vector3), intent(in) :: v
real, intent(in) :: scalar
type(Vector3) :: res

res%x = v%x * scalar
	res%y = v%y * scalar
	res%z = v%z * scalar
	end function vector_scalar_multiply

	subroutine print_vector(v)
	type(Vector3), intent(in) :: v

print *, "(", v%x, ", ", v%y, ", ", v%z, ")"
end subroutine print_vector

interface operator(+)
module procedure vector_add
end interface

interface operator(*)
module procedure vector_scalar_multiply
end interface

end program space_drone


//Explained

program voltage_multiplication_thrust

implicit none

type :: Vector3
real :: x
real :: y
real :: z
contains
procedure :: norm => normalize
procedure :: print => print_vector
end type Vector3

type :: ThrustSystem
real :: inputVoltage
real :: outputVoltage
type(Vector3) :: direction
type(Vector3) :: position
contains
procedure :: generateThrust
procedure :: changeDirection
end type ThrustSystem

type(ThrustSystem) :: thrustSystem
type(Vector3) :: initialDirection
integer :: i

! Initialize the thrust system
thrustSystem%inputVoltage = 24.0
	thrustSystem%outputVoltage = 100000.0
	initialDirection = Vector3(0.0, 1.0, 0.0)
	call thrustSystem%changeDirection(initialDirection)

	! Simulate thrust generation and movement for a few vectors
		do i = 1, 10
	call thrustSystem%generateThrust()
	print *, "Step ", i, ": Position = "
	call thrustSystem%position%print()
	end do

		contains

		subroutine generateThrust(this)
		class(ThrustSystem), intent(inout) :: this
		real :: voltageMultiplier
		type(Vector3) :: thrustVector

		! Simulate voltage multiplication (conceptual)
		voltageMultiplier = this%outputVoltage / this%inputVoltage

			! Simulate thrust vector based on multiplied voltage
			thrustVector = this%direction%norm() * voltageMultiplier
			this%position = this%position + thrustVector

			! Debugging output to visualize the thrust vector
			print *, "Generated Thrust: Vector = "
	call thrustVector%print()
	end subroutine generateThrust

	subroutine changeDirection(this, newDirection)
	class(ThrustSystem), intent(inout) :: this
	type(Vector3), intent(in) :: newDirection

	this%direction = newDirection
	end subroutine changeDirection

	function normalize(v) result(res)
	type(Vector3), intent(in) :: v
type(Vector3) :: res
real :: length

length = sqrt(v%x**2 + v%y**2 + v%z**2)
	res%x = v%x / length
	res%y = v%y / length
	res%z = v%z / length
	end function normalize

	function vector_add(v1, v2) result(res)
	type(Vector3), intent(in) :: v1, v2
type(Vector3) :: res

res%x = v1%x + v2%x
	res%y = v1%y + v2%y
	res%z = v1%z + v2%z
	end function vector_add

	function vector_scalar_multiply(v, scalar) result(res)
	type(Vector3), intent(in) :: v
real, intent(in) :: scalar
type(Vector3) :: res

res%x = v%x * scalar
	res%y = v%y * scalar
	res%z = v%z * scalar
	end function vector_scalar_multiply

	subroutine print_vector(v)
	type(Vector3), intent(in) :: v

print *, "(", v%x, ", ", v%y, ", ", v%z, ")"
end subroutine print_vector

interface operator(+)
module procedure vector_add
end interface

interface operator(*)
module procedure vector_scalar_multiply
end interface

end program voltage_multiplication_thrust

