
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Ccomp_suppliers_viewlist : System.Web.UI.Page
{
    private string Rec = "";
   

    private bool bSort = true;
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    

    protected void Page_Load(object sender, System.EventArgs e)
    {
               
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "comp_suppliers_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;

            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            Session["view_compsuppliers_usr_login"] = vUserId;
            Session["view_compsuppliers_usr_comp"] = PrmComp;
        }
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {

                lblUser.Text = UserLogin.UserName;

            }
        }
	}
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox code = (TextBox)FormView1.FindControl("codeTextBox");
            TextBox reckey = (TextBox)FormView1.FindControl("TextBox1");
            TextBox rec_key = (TextBox)FormView1.FindControl("TextBox2");

            reckey.Text = DateTime.Today.ToString("MMddyyyy") + Convert.ToString(Convert.ToUInt64(Rec) + 1);
            Session["view_list_rec_key"] = reckey.Text;
            rec_key.Text = Convert.ToString(Session["view_list_rec_key"]);
            code.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("nameTextBox");
            code.Focus();
        }
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
       
    protected void lnk_listsupplier_Click(object sender, EventArgs e)
    {
        Response.Redirect("comp_suppliers_list.aspx");
    }
    protected void lnk_viewsupplier_Click(object sender, EventArgs e)
    {
        Response.Redirect("comp_suppliers_viewlist.aspx");

    }
    protected void AddButton_Click(object sender, EventArgs e)
    {
        Label rec = (Label)FormView1.FindControl("Label2");
        Rec = rec.Text;
    }
    protected void lnk_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    protected void Sqldatasource1_Inserted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        TextBox code = (TextBox)FormView1.FindControl("codeTextBox");
        TextBox name = (TextBox)FormView1.FindControl("nameTextBox");
        TextBox add1 = (TextBox)FormView1.FindControl("address1TextBox");
        TextBox add2 = (TextBox)FormView1.FindControl("address2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox rec_key = (TextBox)FormView1.FindControl("TextBox2");
        TextBox rec_key_value = (TextBox)FormView1.FindControl("TextBox1");

        contact_list c1 = new contact_list();
        bool check = c1.validatecomp2(Convert.ToString(Session["view_compsuppliers_usr_comp"]), Convert.ToString(Session["view_compsuppliers_usr_login"]), "Validate", state.Text, zip.Text);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();
                SqlCommand cmd = new SqlCommand("insert into comp_suppliers (comp_code,name, address1, address2, city, state, zip, rec_key ) values('" + code.Text + "','" + name.Text + "','" + add1.Text + "','" + add2.Text + "','" + city.Text + "','" + state.Text + "','" + zip.Text + "','" + rec_key.Text + "')", conn);
                SqlCommand cmd2 = new SqlCommand("insert into contact_reckey (rec_key_value) values ('" + rec_key_value.Text + "')", conn);

                cmd.ExecuteNonQuery();
                cmd2.ExecuteNonQuery();
                conn.Close();
            }
            catch
            {
                return;
            }
            finally
            {
                conn.Close();
                Response.Write("<script> window.location.href='comp_suppliers_list.aspx' </script>");
            }
        }
        else
        {
        }

        
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        Response.Write("<script> window.location.href='comp_suppliers_list.aspx' </script>");
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        TextBox name = (TextBox)FormView1.FindControl("nameTextBox");
        TextBox add1 = (TextBox)FormView1.FindControl("address1TextBox");
        TextBox add2 = (TextBox)FormView1.FindControl("address2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip  = (TextBox)FormView1.FindControl("zipTextBox");

         contact_list c1 = new contact_list();
         bool check = c1.validatecomp2(Convert.ToString(Session["view_compsuppliers_usr_comp"]), Convert.ToString(Session["view_compsuppliers_usr_login"]), "Validate", state.Text, zip.Text);
         string chec = Convert.ToString(check);
        if (chec == "True")
        {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            SqlCommand cmd = new SqlCommand("update comp_suppliers set name='" + name.Text + "',address1='" + add1.Text + "', address2='" + add2.Text + "', city='" + city.Text + "', state='" + state.Text + "', zip='" + zip.Text + "' where comp_code='" + Session["comp_supplier_code"] + "' ", conn);
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            return;
        }
        finally
        {
            conn.Close();
            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        }
        else
        {

        }
    }
    
}

