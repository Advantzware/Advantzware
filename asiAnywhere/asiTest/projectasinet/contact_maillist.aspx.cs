
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Text;
#endregion

public partial class contact_maillist : System.Web.UI.Page
{
    int tot = 0;
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";



    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "contact_list.aspx";
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


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        if (!Page.IsPostBack)
        {
            lblUser.Text = UserLogin.UserName;

            BuildDataSource();
        }



        lblMessage.Text = "";
        dbGrid_contact.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }
        Session["Rowuser"] = UserLogin.UserName;
        //dbGrid_contact.SelectedIndex = Convert.ToInt32(Session["contact_list_index"]);
        //try
        //{
        //    if (Session["contact_list_index"] == null)
        //    {
        //        dbGrid_contact.SelectedIndex = 0;


        //        foreach (GridViewRow gv in dbGrid_contact.Rows)
        //        {
        //            Session["mail_rec_key"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label22")).Text;
        //        }

        //    }
        //}
        //catch
        //{
        //    return;
        //}



        try
        {
            TextBox ddl_display = (TextBox)FormView3.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            if (Convert.ToInt32(ddl_display.Text) > 10)
            {
                //Response.Write("<script>alert('This value should not be greater than 10')</script>");
                Session["size"] = 10;
                ddl_display.Text = "10";
            }
            dbGrid_contact.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }



    }

    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_contact.DataSource = ds;
            dbGrid_contact.DataBind();

            conn.Close();


        }
        catch
        { return; }
        finally
        {
            conn.Close();
        }
    }

    protected void dbGrid_contact_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            e.Row.Cells[36].Visible = false;
            e.Row.Cells[37].Visible = false;
            e.Row.Cells[38].Visible = false;
            e.Row.Cells[39].Visible = false;
            e.Row.Cells[40].Visible = false;
            e.Row.Cells[41].Visible = false;
            e.Row.Cells[42].Visible = false;
            e.Row.Cells[43].Visible = false;
            e.Row.Cells[44].Visible = false;
            e.Row.Cells[45].Visible = false;
            e.Row.Cells[46].Visible = false;
            e.Row.Cells[47].Visible = false;
            e.Row.Cells[48].Visible = false;
            e.Row.Cells[49].Visible = false;
            e.Row.Cells[50].Visible = false;
            e.Row.Cells[51].Visible = false;
            e.Row.Cells[52].Visible = false;
            e.Row.Cells[53].Visible = false;
            e.Row.Cells[54].Visible = false;
            e.Row.Cells[55].Visible = false;
            e.Row.Cells[56].Visible = false;
            e.Row.Cells[57].Visible = false;
            e.Row.Cells[58].Visible = false;
            e.Row.Cells[59].Visible = false;
            e.Row.Cells[60].Visible = false;
        }
        catch { return; }

    }

    protected void dbGrid_contact_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        dbGrid_contact.PageIndex = e.NewPageIndex;
        BuildDataSource();
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

    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txt_city.Text = "";
        txt_company.Text = "";
        txt_state.Text = "";
        txt_tel.Text = "";
        txt_smen.Text = "";
        txt_type.Text = "";
        txt_indsic.Text = "";

        Session["dbGrid_contact_CurrentPageIndex"] = 0;
        Session["dbGrid_contact_SearchSQL"] = null;
        Session["dbGrid_contact_SearchParams"] = null;
        Session["dbGrid_contact_AdvSearch"] = null;
        Session["dbGrid_contact_AdvParam"] = null;
        Session["htPeramcontact"] = null;

        Session["htPeramcontact"] = null;

        Session["contact_list_index"] = null;


        BuildDataSource();
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();


            string cmd = "";
            if (txt_city.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city LIKE '" + txt_city.Text + "%'";

            }
            if (txt_company.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name LIKE '" + txt_company.Text + "%'";
            }
            if (txt_state.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where state LIKE '" + txt_state.Text + "%'";
            }
            if (txt_type.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%'";
            }
            if (txt_tel.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where phone LIKE '" + txt_tel.Text + "%'";
            }
            if (txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where sman LIKE '" + txt_smen.Text + "%'";
            }
            if (txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_indsic.Text + "%'";
            }


            if (txt_city.Text != "" && txt_company.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name LIKE '" + txt_company.Text + "%' and city LIKE '" + txt_city.Text + "%'";
            }
            if (txt_city.Text != "" && txt_state.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where state LIKE '" + txt_state.Text + "%' and city LIKE '" + txt_city.Text + "%'";
            }
            if (txt_city.Text != "" && txt_type.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%' and city LIKE '" + txt_city.Text + "%'";
            }
            if (txt_city.Text != "" && txt_tel.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where phone LIKE '" + txt_tel.Text + "%' and city LIKE '" + txt_city.Text + "%'";
            }
            if (txt_city.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where sman LIKE '" + txt_smen.Text + "%' and city LIKE '" + txt_city.Text + "%'";
            }
            if (txt_city.Text != "" && txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_indsic.Text + "%' and city LIKE '" + txt_city.Text + "%'";
            }



            if (txt_company.Text != "" && txt_state.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name LIKE '" + txt_company.Text + "%' and state LIKE '" + txt_state.Text + "%'";
            }
            if (txt_company.Text != "" && txt_type.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name like '" + txt_company.Text + "%' and type like '" + txt_type.Text + "%'";
            }
            if (txt_company.Text != "" && txt_tel.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name like '" + txt_company.Text + "%' and phone like '" + txt_tel.Text + "%'";
            }
            if (txt_company.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name like '" + txt_company.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }
            if (txt_company.Text != "" && txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name like '" + txt_company.Text + "%' and sic_code like '" + txt_indsic.Text + "%'";
            }


            if (txt_tel.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }
            if (txt_tel.Text != "" && txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where phone like '" + txt_tel.Text + "%' and sic_code like '" + txt_indsic.Text + "%'";
            }
            if (txt_indsic.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where sic_code like '" + txt_indsic.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }

            if (txt_type.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }

            if (txt_indsic.Text != "" && txt_state.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where state LIKE '" + txt_state.Text + "%' and sic_code LIKE '" + txt_indsic.Text + "%'";
            }
            if (txt_indsic.Text != "" && txt_state.Text != "" && txt_city.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where state LIKE '" + txt_state.Text + "%' and sic_code LIKE '" + txt_indsic.Text + "%' and city like '" + txt_city + "%'";
            }

            if (txt_company.Text != "" && txt_tel.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where cust_name like '" + txt_company.Text + "%' and phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }
            if (txt_city.Text != "" && txt_tel.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city like '" + txt_city.Text + "%' and phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }
            if (txt_city.Text != "" && txt_company.Text != "" && txt_tel.Text != "" && txt_smen.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city like '" + txt_city.Text + "%' and cust_name like '" + txt_company.Text + "%' and phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%'";
            }
            if (txt_city.Text != "" && txt_company.Text != "" && txt_tel.Text != "" && txt_smen.Text != "" && txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city like '" + txt_city.Text + "%' and cust_name like '" + txt_company.Text + "%' and phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%' and sic_code like '" + txt_indsic + "%'";
            }
            if (txt_city.Text != "" && txt_state.Text != "" && txt_type.Text != "" && txt_smen.Text != "" && txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city like '" + txt_city.Text + "%' and state like '" + txt_state.Text + "%' and type like '" + txt_type.Text + "%' and sman like '" + txt_smen.Text + "%' and sic_code like '" + txt_indsic + "%'";
            }

            if (txt_city.Text != "" && txt_company.Text != "" && txt_tel.Text != "" && txt_smen.Text != "" && txt_state.Text != "" && txt_type.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city like '" + txt_city.Text + "%' and cust_name like '" + txt_company.Text + "%' and phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%' and state like '" + txt_state.Text + "%' and type like '" + txt_type.Text + "%'";
            }
            if (txt_city.Text != "" && txt_company.Text != "" && txt_tel.Text != "" && txt_smen.Text != "" && txt_state.Text != "" && txt_type.Text != "" && txt_indsic.Text != "")
            {
                cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact where city like '" + txt_city.Text + "%' and cust_name like '" + txt_company.Text + "%' and phone like '" + txt_tel.Text + "%' and sman like '" + txt_smen.Text + "%' and state like '" + txt_state.Text + "%' and type like '" + txt_type.Text + "%' and sic_code like '" + txt_indsic.Text + "%'";
            }

            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_contact.DataSource = ds;
            dbGrid_contact.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            conn.Close();
        }


        Session["dbGrid_contact_CurrentPageIndex"] = 0;



    }

    private void ClearSession()
    {
        Session["dbGrid_contact_Sort"] = null;

        Session["dbGrid_contact_SearchSQL"] = null;
        Session["dbGrid_contact_SearchParams"] = null;

        Session["htPeramcontact"] = null;
        Session["dbGrid_contact_CurrentPageIndex"] = null;
        Session["dbGrid_contact_CurrentPageCount"] = null;

        Session["dbGrid_contact_SortExpression"] = null;
        Session["dbGrid_contact_SortDirection"] = null;
    }

    protected void ShowWait()
    {
        Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
        Response.Write("<script>mydiv.innerText = '';</script>");
        Response.Write("<script language=javascript>;");
        Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
        Response.Write("{var output; output = '" + "Please wait" + "';dots++;if(dots>=dotmax)dots=1;");
        Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
        Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
        Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
        Response.Write("StartShowWait();</script>");
        Response.Flush();
    }


    protected void dbGrid_contact_SelectedIndexChanged(object sender, EventArgs e)
    {
        //Session["contact_list_index"] = dbGrid_contact.SelectedIndex;
        //try
        //{


        //    foreach (GridViewRow gv in dbGrid_contact.Rows)
        //    {
        //        Session["mail_rec_key"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label22")).Text;

        //    }

        //}
        //catch { return; }
    }


    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView3.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        BuildDataSource();
    }

    public SortDirection GridViewSortDirection
    {

        get
        {

            if (ViewState["sortDirection"] == null)

                ViewState["sortDirection"] = SortDirection.Ascending;

            return (SortDirection)ViewState["sortDirection"];

        }

        set { ViewState["sortDirection"] = value; }

    }
    protected void GridView1_Sorting(object sender, GridViewSortEventArgs e)
    {

        string sortExpression = e.SortExpression;

        if (GridViewSortDirection == SortDirection.Ascending)
        {

            GridViewSortDirection = SortDirection.Descending;

            SortGridView(sortExpression, " DESC");

        }

        else
        {

            GridViewSortDirection = SortDirection.Ascending;

            SortGridView(sortExpression, " ASC");

        }

    }
    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }


        Response.Redirect(sMenuURL);
    }


    private void SortGridView(string sortExpression, string direction)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select cust_no as 'Customer',  cust_name as 'Company', last_name as  'Last Name', first_name as 'First Name', addr1 as 'Address',  city as 'City' ,   state as 'State', zip  as 'Zip', phone as 'Phone',   maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc,ship_id ,contact_title ,extension ,type  ,addr2 , country, county, territory, access_code, cell_phone, fax, email, website, comp_code, status_code, sic_code,comp_des,status_des,sic_des, rec_key  from contact";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_contact.DataSource = ds;
            dbGrid_contact.DataBind();
            DataTable dt = ds.Tables[0];

            DataView dv = new DataView(dt);

            dv.Sort = sortExpression + direction;

            dbGrid_contact.DataSource = dv;

            dbGrid_contact.DataBind();


            conn.Close();

        }



        catch { return; }

    }




    protected void lnk_viewcontacts_click(object sender, EventArgs e)
    {
        Response.Redirect("view_contacts.aspx");
    }
    protected void lnk_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    protected void lnk_listcontact_Click(object sender, EventArgs e)
    {
        Response.Redirect("contact_list.aspx");
    }
    protected void lnk_MailList_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_maillist.aspx");
    }

    protected void DeleteAll_click(object sender, EventArgs e)
    {

    }
    protected void PrintButton_Click(object sender, EventArgs e)
    {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < dbGrid_contact.Rows.Count; i++)
        {
            GridViewRow row = dbGrid_contact.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                try
                {
                    tot = tot + 1;
                    str.Append(((Label)dbGrid_contact.Rows[i].FindControl("Label22")).Text);
                    str.Append(",");

                }
                catch
                {
                    Response.Write("Errtor");
                }
            }
        }

        string ss = str.ToString();
        if (tot == 0)
        {
            Session["printone"] = null;

            Session["printtwo"] = null;
            Session["printthree"] = null;
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }



        if (tot == 1)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];

            Session["printtwo"] = null;
            Session["printthree"] = null;
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 2)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = null;
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 3)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 4)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 5)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 6)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 7)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 8)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = radioval[7];
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 9)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = radioval[7];
            Session["printnine"] = radioval[8];
            Session["printten"] = null;
        }
        if (tot == 10)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = radioval[7];
            Session["printnine"] = radioval[8];
            Session["printten"] = radioval[9];
        }
        Response.Write("<script>window.open('printmail.aspx','PrintMail','width=600,height=400')</script>");
    }
    protected void Print3x10Button_Click(object sender, EventArgs e)
    {


        StringBuilder str = new StringBuilder();
        for (int i = 0; i < dbGrid_contact.Rows.Count; i++)
        {
            GridViewRow row = dbGrid_contact.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                try
                {
                    tot = tot + 1;
                    str.Append(((Label)dbGrid_contact.Rows[i].FindControl("Label22")).Text);
                    str.Append(",");

                }
                catch
                {
                    Response.Write("Errtor");
                }
            }
        }

        string ss = str.ToString();
        if (tot == 0)
        {
            Session["printone"] = null;

            Session["printtwo"] = null;
            Session["printthree"] = null;
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }

        if (tot == 1)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];

            Session["printtwo"] = null;
            Session["printthree"] = null;
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 2)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = null;
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 3)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = null;
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 4)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = null;
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 5)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = null;
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 6)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = null;
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 7)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = null;
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 8)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = radioval[7];
            Session["printnine"] = null;
            Session["printten"] = null;
        }
        if (tot == 9)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = radioval[7];
            Session["printnine"] = radioval[8];
            Session["printten"] = null;
        }
        if (tot == 10)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["printone"] = radioval[0];
            Session["printtwo"] = radioval[1];
            Session["printthree"] = radioval[2];
            Session["printfour"] = radioval[3];
            Session["printfive"] = radioval[4];
            Session["printsix"] = radioval[5];
            Session["printseven"] = radioval[6];
            Session["printeight"] = radioval[7];
            Session["printnine"] = radioval[8];
            Session["printten"] = radioval[9];
        }
        Response.Write("<script>window.open('print3x10mail.aspx','Print3x10Mail','width=800,height=650,scrollbars=1')</script>");
        //Response.Redirect("print3x10mail.aspx");
    }
    protected void lnk_calendar_click(object sender, EventArgs e)
    {
        Response.Redirect("appointment.aspx");
    }
}
