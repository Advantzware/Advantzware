
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web.UI;
#endregion

public partial class vendor_aging_rep : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "vendor_aging_rep.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
           
            if (aUsers == "external")
            {
            }
        }
        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;

            try
            {
                string UserId = UserLogin.UserName;
                string aDefaultCust = null;
                string aComp = null;

               // func1 user = new func1();
                //user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                
            }
            catch { }

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'vendor_aging_rep.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begcompTextBox.Text = dr["field1"].ToString();
                    endcompTextBox.Text = dr["field2"].ToString();
                    begvendTextBox.Text = dr["field3"].ToString();
                    endvendTextBox.Text = dr["field4"].ToString();
                    begcurrTextBox.Text = dr["field5"].ToString();
                    endcurrTextBox.Text = dr["field6"].ToString();
                    begtypeTextBox.Text = dr["field7"].ToString();
                    endtypeTextBox.Text = dr["field8"].ToString();
                    asofTextBox.Text = dr["field9"].ToString();
                    day1TextBox.Text = dr["field10"].ToString();
                    day2TextBox.Text = dr["field11"].ToString();
                    day3TextBox.Text = dr["field12"].ToString();
                    day4TextBox.Text = dr["field13"].ToString();

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;



                    if (dr["rd_field1"].ToString() == "Invoice")
                        RadioButtonList1.SelectedIndex = 0;
                    else
                        RadioButtonList1.SelectedIndex = 1;

                    if (dr["rd_field2"].ToString() == "Code")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "Name")
                        RadioButtonList2.SelectedIndex = 1;
                                       
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }

            if (endcompTextBox.Text == "")
                endcompTextBox.Text = "zzz";
            if (endvendTextBox.Text == "")
                endvendTextBox.Text = "zzzzzzzz";
            if (endcurrTextBox.Text == "")
                endcurrTextBox.Text = "zzz";
            if (endtypeTextBox.Text == "")
                endtypeTextBox.Text = "zzzzzzzz";
            
            if (RadioButtonList1.SelectedIndex == -1)
            {
                RadioButtonList1.SelectedIndex = 0;
            }
            if (RadioButtonList2.SelectedIndex == -1)
            {
                RadioButtonList2.SelectedIndex = 0;
            }
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
            RadioButtonList3.SelectedIndex = 0;
        }

            
            
                    
                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
                    }
                    

            
            
        

        
    }

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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {


        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Vendor";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        ObjectDataSource1.SelectParameters["prmBeginCompany"].DefaultValue = begcompTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCompany"].DefaultValue = endcompTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginVendor"].DefaultValue = begvendTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmEndVendor"].DefaultValue = endvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginCurr"].DefaultValue = begcurrTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCurr"].DefaultValue = endcurrTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginType"].DefaultValue = begtypeTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndType"].DefaultValue = endtypeTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAsof"].DefaultValue = asofTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmInvPost"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["prmDay1"].DefaultValue = day1TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDay2"].DefaultValue = day2TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDay3"].DefaultValue = day3TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDay4"].DefaultValue = day4TextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = RadioButtonList2.SelectedValue;
        ObjectDataSource1.SelectParameters["prmDetail"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'vendor_aging_rep.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12,field13, chk_field1,  rd_field1, rd_field2) values ('" + UserLogin.UserName + "','vendor_aging_rep.aspx' , '" + begcompTextBox.Text.Trim() + "', '" + endcompTextBox.Text.Trim() + "','" + begvendTextBox.Text.Trim() + "','" + endvendTextBox.Text.Trim() + "','" + begcurrTextBox.Text.Trim() + "','" + endcurrTextBox.Text.Trim() + "','" + begtypeTextBox.Text.Trim() + "','" + endtypeTextBox.Text.Trim() + "','" + asofTextBox.Text.Trim() + "','" + day1TextBox.Text.Trim() + "','" + day2TextBox.Text.Trim() + "','" + day3TextBox.Text.Trim() + "','" + day4TextBox.Text.Trim() + "','" + Convert.ToString(CheckBox1.Checked) + "','" + RadioButtonList1.SelectedValue + "','" + RadioButtonList2.SelectedValue  + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begcompTextBox.Text.Trim() + "', field2 = '" + endcompTextBox.Text.Trim() + "', field3 = '" + begvendTextBox.Text.Trim() + "', field4 = '" + endvendTextBox.Text.Trim() + "', field5 = '" + begcurrTextBox.Text.Trim() + "', field6 = '" + endcurrTextBox.Text.Trim() + "', field7 = '" + begtypeTextBox.Text.Trim() + "', field8 = '" + endtypeTextBox.Text.Trim() + "', field9 = '" + asofTextBox.Text.Trim() + "', field10 = '" + day1TextBox.Text.Trim() + "', field11 = '" + day2TextBox.Text.Trim() + "', field12 = '" + day3TextBox.Text.Trim() + "', field13 = '" + day4TextBox.Text.Trim() + "', chk_field2 = '" + HiddenField3.Value + "', chk_field3 = '" + HiddenField4.Value + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'vendor_aging_rep.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("vendoragingLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='vendor_aging_rep.aspx'</script>");
            }
        }
        catch { }


    }

       

}
