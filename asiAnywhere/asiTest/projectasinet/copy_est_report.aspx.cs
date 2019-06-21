#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web;
#endregion

public partial class copy_est_list : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_estimate.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {            
        
            if (Session["User"] != null)
            {                
                lblUser.Text = UserLogin.UserName;
            }


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'copy_est_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    fromcompTextBox.Text = dr["field1"].ToString();
                    frmcompnamLabel.Text = dr["field2"].ToString();
                    BegEstTextBox.Text = dr["field3"].ToString();
                    shipnamLabel.Text = dr["field4"].ToString();
                    tocompTextBox.Text = dr["field5"].ToString();
                    tocompnamlabel.Text = dr["field6"].ToString();
                    itmnamLabel.Text = dr["field7"].ToString();
                    BegCustTextBox.Text = dr["field8"].ToString();
                    custpartTextBox.Text = dr["field9"].ToString();                    

                    if (dr["chk_field1"].ToString() == "Yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "Yes")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    
                    if (dr["chk_field3"].ToString() == "Yes")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["chk_field4"].ToString() == "Yes")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;
                    if (dr["chk_field5"].ToString() == "Yes")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;
                    if (dr["chk_field6"].ToString() == "Yes")
                        CheckBox6.Checked = true;
                    else
                        CheckBox6.Checked = false;
                    if (dr["chk_field7"].ToString() == "Yes")
                        CheckBox7.Checked = true;
                    else
                        CheckBox7.Checked = false;
                    if (dr["chk_field8"].ToString() == "Yes")
                        CheckBox8.Checked = true;
                    else
                        CheckBox8.Checked = false;
                    if (dr["chk_field9"].ToString() == "Yes")
                        CheckBox9.Checked = true;
                    else
                        CheckBox9.Checked = false;
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (CheckBox1.Checked)     
            HiddenField1.Value = "Yes";      
        else        
            HiddenField1.Value = "No";
        if (CheckBox2.Checked)
            HiddenField2.Value = "Yes";
        else
            HiddenField2.Value = "No";
        if (CheckBox3.Checked)
            HiddenField3.Value = "Yes";
        else
            HiddenField3.Value = "No";
        if (CheckBox4.Checked)
            HiddenField4.Value = "Yes";
        else
            HiddenField4.Value = "No";

        if (CheckBox5.Checked)
            HiddenField5.Value = "Yes";
        else
            HiddenField5.Value = "No";
        if (CheckBox6.Checked)
            HiddenField6.Value = "Yes";
        else
            HiddenField6.Value = "No";
        if (CheckBox7.Checked)
            HiddenField7.Value = "Yes";
        else
            HiddenField7.Value = "No";
        if (CheckBox8.Checked)
            HiddenField8.Value = "Yes";
        else
            HiddenField8.Value = "No";
        if (CheckBox9.Checked)
            HiddenField9.Value = "Yes";
        else
            HiddenField9.Value = "No";

         string value = "";
         Label1.Text = "";
                Corrugated vcheck = new Corrugated();
                bool check = vcheck.ValidateCopyEstimate("CheckValidate", UserLogin.UserName, fromcompTextBox.Text, BegEstTextBox.Text, Convert.ToString(HiddenField1.Value), Convert.ToString(HiddenField2.Value), Convert.ToString(HiddenField3.Value), Convert.ToString(HiddenField4.Value), Convert.ToString(HiddenField9.Value), Convert.ToString(HiddenField5.Value), tocompTextBox.Text, EndEstLabel.Text, BegCustTextBox.Text, custpartTextBox.Text, Convert.ToString(HiddenField6.Value), Convert.ToString(HiddenField7.Value), Convert.ToString(HiddenField8.Value));
                value = Convert.ToString(check);


                if (value == "True")
                {

                    Corrugated corr = new Corrugated();
                    DataSet ds = new DataSet();
                    ds = corr.SelectCopyEstimate("RunProcess", UserLogin.UserName, fromcompTextBox.Text, BegEstTextBox.Text, Convert.ToString(HiddenField1.Value), Convert.ToString(HiddenField2.Value), Convert.ToString(HiddenField3.Value), Convert.ToString(HiddenField4.Value), Convert.ToString(HiddenField9.Value), Convert.ToString(HiddenField5.Value), tocompTextBox.Text, EndEstLabel.Text, BegCustTextBox.Text, custpartTextBox.Text, Convert.ToString(HiddenField6.Value), Convert.ToString(HiddenField7.Value), Convert.ToString(HiddenField8.Value));
                    if (ds.Tables[0].Rows.Count > 0)
                    {
                        HttpContext.Current.Response.Write("<script>alert('" + "Process Is Completed." + "')</script>");
                        EndEstLabel.Text = ds.Tables[0].Rows[0][0].ToString();
                    }
                }
         SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'copy_est_report.aspx' ";
                    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    if (ds.Tables[0].Rows.Count == 0)
                    {
                        SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, chk_field1, chk_field2, chk_field3, chk_field4,chk_field5,chk_field6,chk_field7,chk_field8,chk_field9) values ('" + UserLogin.UserName + "','copy_est_report.aspx' , '" + fromcompTextBox.Text.Trim() + "', '" + frmcompnamLabel.Text.Trim() + "', '" + BegEstTextBox.Text.Trim() + "', '" + shipnamLabel.Text.Trim() + "', '" + tocompTextBox.Text.Trim() + "', '" + tocompnamlabel.Text.Trim() + "', '" + itmnamLabel.Text + "', '" + BegCustTextBox.Text.Trim() + "', '" + custpartTextBox.Text.Trim() + "', '" + Convert.ToString(HiddenField1.Value) + "', '" + Convert.ToString(HiddenField2.Value) + "', '" + Convert.ToString(HiddenField3.Value) + "', '" + Convert.ToString(HiddenField4.Value) + "', '" + HiddenField5.Value + "', '" + HiddenField6.Value + "', '" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "')", conn);
                        cmd_insert.ExecuteNonQuery();
                    }
                    else
                    {
                        SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + fromcompTextBox.Text.Trim() + "', field2 = '" + frmcompnamLabel.Text.Trim() + "', field3 = '" + BegEstTextBox.Text.Trim() + "', field4 = '" + shipnamLabel.Text.Trim() + "', field5 = '" + tocompTextBox.Text.Trim() + "', field6 = '" + tocompnamlabel.Text.Trim() + "', field7 = '" + itmnamLabel.Text + "', field8 = '" + BegCustTextBox.Text.Trim() + "', field9 = '" + custpartTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "',chk_field5 = '" + HiddenField5.Value + "',chk_field6 = '" + HiddenField6.Value + "',chk_field7 = '" + HiddenField7.Value + "',chk_field8 = '" + HiddenField8.Value + "',chk_field9 = '" + HiddenField9.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'copy_est_report.aspx' ", conn);
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


              
    }

    protected void BegEstTextBox_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        LookUp ord = new LookUp();
        DataSet ds = new DataSet();
        ds = ord.SelectEstimateLookup("search", UserLogin.UserName, "Estimate", "BEGIN", BegEstTextBox.Text);
        if (ds.Tables[0].Rows.Count > 0)
        {
            itmnamLabel.Text = ds.Tables[0].Rows[0][2].ToString();
            shipnamLabel.Text = ds.Tables[0].Rows[0][3].ToString();
            BegCustTextBox.Text = ds.Tables[0].Rows[0][1].ToString();
            custpartTextBox.Text = ds.Tables[0].Rows[0][4].ToString();
        }
        else
        {
            //HttpContext.Current.Response.Write("<script>alert('" + "Invalid Estimate, Try Help" + "')</script>");
            Label1.Text = "Invalid Estimate, Try Help";
            BegEstTextBox.Focus();
        }
        
    }
    protected void FromComp_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        LookUp ord = new LookUp();
        DataSet ds = new DataSet();
        ds = ord.SelCompLook("search", UserLogin.UserName, "company", "BEGIN", fromcompTextBox.Text);
        if (ds.Tables[0].Rows.Count > 0)
        {
            frmcompnamLabel.Text = ds.Tables[0].Rows[0][1].ToString();
        }
        else
        {
            //HttpContext.Current.Response.Write("<script>alert('" + "Invalid From Company, Try Help" + "')</script>");
            Label1.Text = "Invalid From Company, Try Help";
            fromcompTextBox.Focus();
        }

    }
    protected void toComp_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        LookUp ord = new LookUp();
        DataSet ds = new DataSet();
        ds = ord.SelCompLook("search", UserLogin.UserName, "company", "BEGIN", tocompTextBox.Text);
        if (ds.Tables[0].Rows.Count > 0)
        {
            tocompnamlabel.Text = ds.Tables[0].Rows[0][1].ToString();
        }
        else
        {
            //HttpContext.Current.Response.Write("<script>alert('" + "Invalid To Company, Try Help" + "')</script>");
            Label1.Text = "Invalid To Company, Try Help";
            tocompTextBox.Focus();
        }

    }

   }
